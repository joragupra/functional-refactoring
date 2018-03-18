package com.joragupra.validators

import com.netaporter.uri.Uri
import com.netaporter.uri.Uri.parse
import com.netaporter.uri.config.UriConfig
import scalaz.{-\/, \/, \/-}

case class UrlValidationSuccess(validatedUrl: String)
case class UrlValidationError(message: String)

class UrlValidator {

  private implicit val config = UriConfig.conservative

  def validate(urlString: String): \/[UrlValidationError, UrlValidationSuccess] = {
    try {
      val uri = parse(urlString)

      for {
        _ <- validateLength2(uri)
        _ <- validateProtocol2(uri)
        _ <- validateIsNotLocalhost2(uri)
        _ <- validateNoUserAndPassword2(uri)
        r <- validateIsNotAnIPv4Address2(uri)
      } yield {
        r
      }
    } catch {
      case _: java.net.URISyntaxException => -\/(UrlValidationError("Url does not have a valid structure"))
    }
  }

  def validateLength2(uri: Uri): \/[UrlValidationError, UrlValidationSuccess] = {
    val urlString = uri.toStringRaw
    if (urlString.length() <= 300) {
      \/-(UrlValidationSuccess(uri.toString))
    }
    else {
      -\/(UrlValidationError("Url too long"))
    }
  }

  def validateLength(uri: Uri): Boolean = {
    val urlString = uri.toStringRaw
    if (urlString.length() <= 300) {
      true
    }
    else {
      false
    }
  }

  def validateProtocol2(uri: Uri): \/[UrlValidationError, UrlValidationSuccess] = {
    uri.protocol match {
      case Some(protocol) if protocol.equalsIgnoreCase("http") || protocol.equalsIgnoreCase("https") => \/-(UrlValidationSuccess(uri.toString))
      case _ => -\/(UrlValidationError("Url does not have a supported protocol"))
    }
  }

  def validateProtocol(uri: Uri): Boolean = {
    uri.protocol match {
      case Some(protocol) if protocol.equalsIgnoreCase("http") || protocol.equalsIgnoreCase("https") => true
      case _ => false
    }
  }

  def validateIsNotAnIPv4Address2(uri: Uri): \/[UrlValidationError, UrlValidationSuccess] = {
    uri.host match {
      case Some(host) if host.matches("[0-9.]+") => -\/(UrlValidationError("Url can not contain IPv4 addresses"))
      case _ => \/-(UrlValidationSuccess(uri.toString))
    }
  }

  def validateIsNotAnIPv4Address(uri: Uri): Boolean = {
    uri.host match {
      case Some(host) if host.matches("[0-9.]+") => false
      case _ => true
    }
  }

  def validateIsNotLocalhost2(uri: Uri): \/[UrlValidationError, UrlValidationSuccess] = {
    val urlToLocalhost = uri.host.contains("localhost")
    if (!urlToLocalhost) {
      \/-(UrlValidationSuccess(uri.toString))
    } else {
      -\/(UrlValidationError("Url should not point to localhost"))
    }
  }

  def validateIsNotLocalhost(uri: Uri): Boolean = {
    val urlToLocalhost = uri.host.contains("localhost")
    if (!urlToLocalhost) {
      true
    } else {
      false
    }
  }

  def validateNoUserAndPassword2(uri: Uri): \/[UrlValidationError, UrlValidationSuccess] = {
    (uri.user, uri.password) match {
      case (Some(_), None) => -\/(UrlValidationError("Url can't contain user and password"))
      case (Some(_), Some(_)) => -\/(UrlValidationError("Url can't contain user and password"))
      case _ => \/-(UrlValidationSuccess(uri.toString))
    }
  }

  def validateNoUserAndPassword(uri: Uri): Boolean = {
    (uri.user, uri.password) match {
      case (Some(_), None) => false
      case (Some(_), Some(_)) => false
      case _ => true
    }
  }

}

