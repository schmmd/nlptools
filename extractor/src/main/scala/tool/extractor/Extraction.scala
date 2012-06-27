package edu.washington.cs.knowitall
package tool.extractor

import edu.washington.cs.knowitall.collection.immutable.Interval
import tool.stem.Lemmatized
import tool.chunk.ChunkedToken

case class ExtractionPart(interval: Interval, text: String) {
  def this(tokens: Seq[Lemmatized[ChunkedToken]], interval: Interval) =
    this(interval, tokens.view(interval.start, interval.end).iterator.map(_.token.string).mkString(" "))
    
  override def toString = text
}

case class BinaryExtraction(arg1: ExtractionPart, rel: ExtractionPart, arg2: ExtractionPart) {
  override def toString = Iterable(arg1, rel, arg2).mkString("(", "; ", ")")
}