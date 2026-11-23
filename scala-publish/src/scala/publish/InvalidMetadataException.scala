package scala.publish

/** Thrown when a maven-metadata.xml file can't be understood */
final class InvalidMetadataException(message: String) extends Exception(message)
