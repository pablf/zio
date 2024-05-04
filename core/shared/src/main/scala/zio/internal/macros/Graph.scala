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
  var neededKeys: Map[Key, Int] = Map.empty
  // Dependencies to pass to next iteration of buildComplete
  private var dependencies: List[Key] = Nil
  private var envDependencies: List[Key] = Nil

  def buildNodes(outputs: List[Key], sideEffectNodes: List[Node[Key, A]]): Either[::[GraphError[Key, A]], LayerTree[A]] = for {
    _ <- Right(println(s"called with ${outputs.toString} and ${nodes.toString} and ${sideEffectNodes.toString} "))
    _           <- neededKeys((outputs ++ sideEffectNodes.flatMap(_.inputs)).distinct)
    sideEffects <- forEach(sideEffectNodes)(buildNode).map(_.combineHorizontally)
    rightTree   <- build(outputs)
    leftTree    <- buildComplete(constructDeps())
  } yield leftTree >>> (rightTree ++ sideEffects)


  private def buildComplete(outputs: List[Key]): Either[::[GraphError[Key, A]], LayerTree[A]] =
    if (!outputs.isEmpty)
      for {
        _         <- Right(restartKeys())
        _         <- neededKeys(outputs)
        rightTree <- build(outputs)
        leftTree  <- buildComplete(constructDeps())
      } yield leftTree >>> rightTree
    else Right(LayerTree.empty)

  private def constructDeps(): List[Key] = {
    if (dependencies.isEmpty) {println("emptyDeps"); dependencies}
    else {println(dependencies); dependencies.distinct ++ envDependencies.distinct}
  }

  /**
   * Restarts variables for next iteration of buildComplete
   */
  private def restartKeys(): Unit = {
    neededKeys = Map.empty
    dependencies = Nil
  }

  /**
   * Initializes neededKeys
   */
  def neededKeys(
    outputs: List[Key],
    seen: Set[Node[Key, A]] = Set.empty,
    parent: Option[Node[Key, A]] = None
  ): Either[::[GraphError[Key, A]], Unit] = {
    var created: List[Key] = Nil

    forEach(outputs) { output =>
      if (created.contains(output)) Right(())
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
        _ <- neededKeys(node.inputs, seen + node, Some(node))
      } yield ()
      }
    }.map(_ => ())
  }
    
  private def addEnv(key: Key): Unit =
    neededKeys.get(key) match {
      case Some(_) => ()
      case None    => neededKeys = neededKeys + (key -> -1)
    }

  private def addKey(key: Key): Unit =
    neededKeys.get(key) match {
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
      else neededKeys.get(output) match {
        case None => Right(LayerTree.empty)
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
      else neededKeys.get(output) match {
        case None => Right((LayerTree.empty, true))
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
    if (isEnv(output)) Right(environment(output))
    else nodes.find(_.outputs.exists(keyEquals(_, output))).toRight(::(error, Nil))

  private def isEnv(key: Key): Boolean =
    envKeys.exists(env => keyEquals(env, key))

  private def buildNode(
    node: Node[Key, A],
    seen: Set[Node[Key, A]]
  ): Either[::[GraphError[Key, A]], LayerTree[A]] =
    forEach(node.inputs) { input =>
      for {
        out <- getNodeWithOutput(input, error = GraphError.missingTransitiveDependency(node, input))
        _   <- assertNonCircularDependency(node, seen, out)
        result <- 
          if (isEnv(input)) {
            envDependencies = input :: envDependencies
            Right((LayerTree.succeed(environment(input).value), true))
          } else neededKeys.get(input) match {
                    case None    => Left(::(GraphError.missingTransitiveDependency(node, input), Nil))
                    case Some(1) => buildNode(out, seen + out).map(tree => (tree, false))
                    case Some(n) => {
                      dependencies = input :: dependencies
                      Right((LayerTree.succeed(environment(input).value), true))
                    }
                  }
      } yield result
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