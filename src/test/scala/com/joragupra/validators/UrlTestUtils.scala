package com.joragupra.validators

object UrlTestUtils {

  import org.scalacheck.Gen

  val validator = new UrlValidator

  val stringWithLengthGenerator: (Int) => Gen[String] = (n: Int) => Gen.listOfN(n, Gen.alphaNumChar).map(_.mkString)

  val stringNumWithLengthGenerator: (Int) => Gen[String] = (n: Int) => Gen.listOfN(n, Gen.numChar).map(_.mkString)

  val stringsWithLengthFromToGenerator: (Int, Int) => Gen[String] = (n: Int, m: Int) => for {
    i <- Gen.choose(n, m)
    s <- stringWithLengthGenerator(i)
  } yield s

  val stringsWithLengthFromToGeneratorNotContainingGenerator: (Int, Int, Seq[String]) => Gen[String] = (n: Int, m: Int, notIncluding: Seq[String]) => for {
    i <- Gen.choose(n, m)
    s <- stringWithLengthGenerator(i) if !notIncluding.exists(notIncluded => s.contains(notIncluded))
  } yield s

  val ipv4Generator: Gen[String] = for {
    firstPart <- stringNumWithLengthGenerator(3)
    secondPart <- stringNumWithLengthGenerator(3)
    thirdPart <- stringNumWithLengthGenerator(3)
  } yield firstPart + "." + secondPart + "." + thirdPart

  def withHttpProtocol(address: String): String = "http://" + address

  def withHttpsProtocol(address: String): String = "https://" + address

}
