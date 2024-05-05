package zio.internal.macros

import zio.internal.macros.LayerTree._

final case class Graph[Key, A](
  nodes: List[Node[Key, A]],
  keyEquals: (Key, Key) => Boolean,
  environment: Key => Node[Key, A],
  envKeys: List[Key],
) {

  // Map assigning to each type the times that it must be built
  // -1 designs a `Key` from the environment
  privatevar neededKeys: Map[Key, Int] = Map.empty
  // Dependencies to pass to next iteration of buildComplete
  private var dependencies: List[Key] = Nil
  private var envDependencies: List[Key] = Nil


  private var usedRemainders: Set[Key] = Set.empty

  def usedRemainders(): Set[A] = usedRemainders.map(environment(_)).map(_.value)

  def buildNodes(outputs: List[Key], sideEffectNodes: List[Node[Key, A]]): Either[::[GraphError[Key, A]], LayerTree[A]] = for {
    _           <- mkNeededKeys(outputs ++ sideEffectNodes.flatMap(_.inputs))
    sideEffects <- forEach(sideEffectNodes)(buildNode).map(_.combineHorizontally)
    rightTree   <- build(outputs)
    leftTree    <- buildComplete(constructDeps())
  } yield leftTree >>> (rightTree ++ sideEffects)


  private def buildComplete(outputs: List[Key]): Either[::[GraphError[Key, A]], LayerTree[A]] =
    if (!outputs.isEmpty)
      for {
        _         <- Right(restartKeys())
        _         <- mkNeededKeys(outputs)
        rightTree <- build(outputs)
        leftTree  <- buildComplete(constructDeps())
      } yield leftTree >>> rightTree
    else Right(LayerTree.empty)

  private def constructDeps(): List[Key] = {
    if (dependencies.isEmpty) dependencies
    else distinctKeys(dependencies) ++ distinctKeys(envDependencies)
  }


  /**
   * Restarts variables for next iteration of buildComplete
   */
  private def restartKeys(): Unit = {
    neededKeys = Map.empty
    dependencies = Nil
  }

  private def distinctKeys(keys: List[Key]): List[Key] = {
    var distinct: List[Key] = List.empty
    for (k <- keys) {
      if (!distinct.exists(k2 => keyEquals(k, k2)||keyEquals(k2, k))) distinct = k :: distinct
    }
    distinct.reverse
  }

  /**
   * Initializes neededKeys
   */
  def mkNeededKeys(
    outputs: List[Key],
    seen: Set[Node[Key, A]] = Set.empty,
    parent: Option[Node[Key, A]] = None
  ): Either[::[GraphError[Key, A]], Unit] = {
    var created: List[Key] = Nil

    forEach(outputs) { output =>
      if (created.exists(k => keyEquals(output, k)||keyEquals(k, output))) Right(())
      else if(isEnv(output)) Right(addEnv(output))
      else {
        for {
        node <- parent match {
                  case Some(p) =>
                    getNodeWithOutput[GraphError[Key, A]](
                      output,
                      error = GraphError.missingTransitiveDependency(p, output)
                    )
                  case None =>
                    getNodeWithOutput[GraphError[Key, A]](output, error = GraphError.MissingTopLevelDependency(output))
                }
        _ <- Right(node.outputs.map(addKey(_)))
        _ <- Right{created = node.outputs ++ created}
        _ <- parent match {
               case Some(p) => assertNonCircularDependency(p, seen, node)
               case None    => Right(())
             }
        _ <- mkNeededKeys(node.inputs, seen + node, Some(node))
      } yield ()
      }
    }

    Right(())
  }

  private def getKey(key: Key): Option[Int] = {
    neededKeys.get(key) match {
      case Some(n) => Some(n)
      case None => neededKeys.keySet.find(k => keyEquals(k, key)||keyEquals(key, k)) match {
        case Some(aliasKey) => neededKeys.get(aliasKey)
        case None => None
      }
    }
  }
    
  private def addEnv(key: Key): Unit = {
    usedRemainders = usedRemainders + key
    getKey(key) match {
      case Some(_) => ()
      case None    => neededKeys = neededKeys + (key -> -1)
    }
  }
  private def addKey(key: Key): Unit =
    getKey(key) match {
      case Some(-1) => ()
      case Some(n) => neededKeys = neededKeys + (key -> (n + 1))
      case None    => neededKeys = neededKeys + (key -> 1)
    }

  /**
   * Builds a layer containing only types that appears once. Types appearing
   * more than once are replaced with ZLayer.environment[_] and left for the
   * next iteration of buildComplete to create.
   */
  private def build(outputs: List[Key]): Either[::[GraphError[Key, A]], LayerTree[A]] = 
    forEach(outputs) { output =>
      if(isEnv(output)) {
        envDependencies = output :: envDependencies
        Right(LayerTree.succeed(environment(output).value))
      }
      else getKey(output) match {
        case None => throw new Throwable(s"This shouldn't happen")
        case Some(1) =>
          getNodeWithOutput[GraphError[Key, A]](output, error = GraphError.MissingTopLevelDependency(output))
            .flatMap(node => buildNode(node, Set(node)))
        case Some(n) => {
          dependencies = output :: dependencies
          Right(LayerTree.succeed(environment(output).value))
        }
      }
    }
    .map(_.distinct.combineHorizontally)




  private def buildNode(node: Node[Key, A]): Either[::[GraphError[Key, A]], LayerTree[A]] =
    forEach(node.inputs) { output =>
      if(isEnv(output)) {
        envDependencies = output :: envDependencies
        Right((LayerTree.succeed(environment(output).value), true))
      }
      else getKey(output) match {
        case None => throw new Throwable(s"This shouldn't happen")
        case Some(1) =>
          getNodeWithOutput[GraphError[Key, A]](output, error = GraphError.MissingTopLevelDependency(output))
            .flatMap(node => buildNode(node, Set(node)).map(tree => (tree, false)))
        case Some(n) => {
          dependencies = output :: dependencies
          Right((LayerTree.succeed(environment(output).value), true))
        }
      }
    }
    .map { deps =>
      if (deps.forall(_._2)) LayerTree.succeed(node.value)
      else deps.map(_._1).distinct.combineHorizontally >>> LayerTree.succeed(node.value)
    }

  def map[B](f: A => B): Graph[Key, B] =
    Graph(nodes.map(_.map(f)), keyEquals, key => environment(key).map(f), envKeys)

  private def getNodeWithOutput[E](output: Key, error: E): Either[::[E], Node[Key, A]] =
    if (isEnv(output)) throw new Throwable(s"This shouldn't happen")
    else nodes.find(_.outputs.exists(keyEquals(_, output))).toRight(::(error, Nil))

  private def isEnv(key: Key): Boolean =
    envKeys.exists(env => keyEquals(env, key))

  private def buildNode(
    node: Node[Key, A],
    seen: Set[Node[Key, A]]
  ): Either[::[GraphError[Key, A]], LayerTree[A]] =
    forEach(node.inputs) { input =>
      if (isEnv(input)) {
            envDependencies = input :: envDependencies
            Right((LayerTree.succeed(environment(input).value), true))
      } else getKey(input) match {
                    case None    => Left(::(GraphError.missingTransitiveDependency(node, input), Nil))
                    case Some(1) => {
                      for {
                        out <- getNodeWithOutput(input, error = GraphError.missingTransitiveDependency(node, input))
                        _   <- assertNonCircularDependency(node, seen, out)
                        result <- buildNode(out, seen + out).map(tree => (tree, false))
                      } yield result
                    }
                    case Some(n) => {
                      dependencies = input :: dependencies
                      Right((LayerTree.succeed(environment(input).value), true))
          }
        }
    }.map { deps =>
      if (deps.forall(_._2)) LayerTree.succeed(node.value)
      else deps.map(_._1).distinct.combineHorizontally >>> LayerTree.succeed(node.value)
    }

  private def assertNonCircularDependency(
    node: Node[Key, A],
    seen: Set[Node[Key, A]],
    dependency: Node[Key, A]
  ): Either[::[GraphError[Key, A]], Unit] =
    if (seen(dependency))
      Left(::(GraphError.CircularDependency(node, dependency, seen.size), Nil))
    else
      Right(())

  private def forEach[B, C](
    list: List[B]
  )(f: B => Either[::[GraphError[Key, A]], C]): Either[::[GraphError[Key, A]], List[C]] =
    list.foldRight[Either[::[GraphError[Key, A]], List[C]]](Right(List.empty)) { (a, b) =>
      (f(a), b) match {
        case (Left(::(e, es)), Left(e1s)) => Left(::(e, es ++ e1s))
        case (Left(es), _)                => Left(es)
        case (_, Left(es))                => Left(es)
        case (Right(a), Right(b))         => Right(a +: b)
      }
    }
}