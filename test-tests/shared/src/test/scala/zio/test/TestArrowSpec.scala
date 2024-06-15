package zio.test

object TestArrowSpec extends ZIOBaseSpec {

  import TestArrow._

  def createMeta(
    span: Option[Span] = None,
    parentSpan: Option[Span] = None,
    code: Option[String] = None,
    location: Option[String] = None,
    completeCode: Option[String] = None,
    customLabel: Option[String] = None,
    genFailureDetails: Option[GenFailureDetails] = None
  ) =
      new Meta(
        TestArrowF[Any, Nothing](_ =>TestTrace.fail),
        span,
        parentSpan,
        code,
        location,
        completeCode,
        customLabel,
        genFailureDetails
      )

  def spec =
    suite("TestArrowSpec")(
      suite(".meta")(
        test("change None span") {
          val span = Some(Span(0, 1))
          val meta = createMeta(span = None)
          val res1 = meta.meta(span = None).asInstanceOf[Meta[Any, Nothing]].span
          val res2 = meta.meta(span = span).asInstanceOf[Meta[Any, Nothing]].span
          assertTrue(res1 == None && res2 == span)
        },
        test("change Some span") {
          val span1 = Some(Span(0, 1))
          val span2 = Some(Span(2, 3))
          val meta = createMeta(span = span1)
          assertTrue(meta.meta(span = None).asInstanceOf[Meta[Any, Nothing]].span == span1 && meta.meta(span = span2).asInstanceOf[Meta[Any, Nothing]].span == span2)
        },
        test("change None parentSpan") {
          val parentSpan = Some(Span(0, 1))
          val meta = createMeta(span = None)
          assertTrue(meta.meta(span = None).asInstanceOf[Meta[Any, Nothing]].span == None && meta.meta(span = parentSpan).asInstanceOf[Meta[Any, Nothing]].span == parentSpan)
        },
        test("change Some parentSpan") {
          val parentSpan1 = Some(Span(0, 1))
          val parentSpan2 = Some(Span(2, 3))
          val meta = createMeta(span = parentSpan1)
          assertTrue(meta.meta(span = None).asInstanceOf[Meta[Any, Nothing]].span == parentSpan1 && meta.meta(span = parentSpan2).asInstanceOf[Meta[Any, Nothing]].span == parentSpan2)
        },
        test("change None code") {
          val code = Some("some code")
          val meta = createMeta(code = None)
          assertTrue(meta.meta(code = None).asInstanceOf[Meta[Any, Nothing]].code == None && meta.meta(code = code).asInstanceOf[Meta[Any, Nothing]].code == code)
        },
        test("change Some code") {
          val code1 = Some("some code")
          val code2 = Some("other code")
          val meta = createMeta(code = code1)
          assertTrue(meta.meta(code = None).asInstanceOf[Meta[Any, Nothing]].code == code1 && meta.meta(code = code2).asInstanceOf[Meta[Any, Nothing]].code == code2)
        },
        test("change None location") {
          val location = Some("some location")
          val meta = createMeta(location = None)
          assertTrue(meta.meta(location = None).asInstanceOf[Meta[Any, Nothing]].location == None && meta.meta(location = location).asInstanceOf[Meta[Any, Nothing]].location == location)
        },
        test("change Some location") {
          val location1 = Some("some location")
          val location2 = Some("other location")
          val meta = createMeta(location = location1)
          assertTrue(meta.meta(location = None).asInstanceOf[Meta[Any, Nothing]].location == location1 && meta.meta(location = location2).asInstanceOf[Meta[Any, Nothing]].location == location2)
        },
        test("change None completeCode") {
          val completeCode = Some("some completeCode")
          val meta = createMeta(completeCode = None)
          assertTrue(meta.meta(completeCode = None).asInstanceOf[Meta[Any, Nothing]].completeCode == None && meta.meta(completeCode = completeCode).asInstanceOf[Meta[Any, Nothing]].completeCode == completeCode)
        },
        test("change Some completeCode") {
          val completeCode1 = Some("some completeCode")
          val completeCode2 = Some("other completeCode")
          val meta = createMeta(completeCode = completeCode1)
          assertTrue(meta.meta(completeCode = None).asInstanceOf[Meta[Any, Nothing]].completeCode == completeCode1 && meta.meta(completeCode = completeCode2).asInstanceOf[Meta[Any, Nothing]].completeCode == completeCode2)
        },
        test("change None customLabel") {
          val customLabel = Some("some customLabel")
          val meta = createMeta(customLabel = None)
          assertTrue(meta.meta(customLabel = None).asInstanceOf[Meta[Any, Nothing]].customLabel == None && meta.meta(customLabel = customLabel).asInstanceOf[Meta[Any, Nothing]].customLabel == customLabel)
        },
        test("change Some customLabel") {
          val customLabel1 = Some("some customLabel")
          val customLabel2 = Some("other customLabel")
          val meta = createMeta(customLabel = customLabel1)
          assertTrue(meta.meta(customLabel = None).asInstanceOf[Meta[Any, Nothing]].customLabel == customLabel1 && meta.meta(customLabel = customLabel2).asInstanceOf[Meta[Any, Nothing]].customLabel == customLabel2)
        },
        test("change None genFailureDetails") {
          val genFailureDetails = Some(GenFailureDetails(None, None, 1))
          val meta = createMeta(genFailureDetails = None)
          assertTrue(meta.meta(genFailureDetails = None).asInstanceOf[Meta[Any, Nothing]].genFailureDetails == None && meta.meta(genFailureDetails = genFailureDetails).asInstanceOf[Meta[Any, Nothing]].genFailureDetails.map(_.iterations == 1).getOrElse(false))
        },
        test("change Some genFailureDetails") {
          val genFailureDetails1 = Some(GenFailureDetails(None, None, 1))
          val genFailureDetails2 = Some(GenFailureDetails(None, None, 2))
          val meta = createMeta(genFailureDetails = genFailureDetails1)
          assertTrue(meta.meta(genFailureDetails = None).asInstanceOf[Meta[Any, Nothing]].genFailureDetails.map(_.iterations == 1).getOrElse(false) && meta.meta(genFailureDetails = genFailureDetails2).asInstanceOf[Meta[Any, Nothing]].genFailureDetails.map(_.iterations == 2).getOrElse(false))
        },
      ),
    )

}
