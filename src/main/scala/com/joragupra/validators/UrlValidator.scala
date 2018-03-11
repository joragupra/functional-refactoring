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
      if(validateLength(parse(urlString))) {
        if(validateProtocol(parse(urlString))) {
          if(validateIsNotLocalhost(parse(urlString))) {
            if(validateNoUserAndPassword(parse(urlString))) {
              if(validateIsNotAnIPv4Address(parse(urlString))) {
                \/-(UrlValidationSuccess(parse(urlString).toString))
              } else -\/(UrlValidationError("Url can not contain IPv4 addresses"))
            } else -\/(UrlValidationError("Url can't contain user and password"))
          } else -\/(UrlValidationError("Url should not point to localhost"))
        } else -\/(UrlValidationError("Url does not have a supported protocol"))
      } else -\/(UrlValidationError("Url too long"))
    } catch {
      case _: java.net.URISyntaxException => -\/(UrlValidationError("Url does not have a valid structure"))
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

  def validateProtocol(uri: Uri): Boolean = {
    uri.protocol match {
      case Some(protocol) if protocol.equalsIgnoreCase("http") || protocol.equalsIgnoreCase("https") => true
      case _ => false
    }
  }

  def validateIsNotAnIPv4Address(uri: Uri): Boolean = {
    uri.host match {
      case Some(host) if host.matches("[0-9.]+") => false
      case _ => true
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

  def validateNoUserAndPassword(uri: Uri): Boolean = {
    (uri.user, uri.password) match {
      case (Some(_), None) => false
      case (Some(_), Some(_)) => false
      case _ => true
    }
  }

}

