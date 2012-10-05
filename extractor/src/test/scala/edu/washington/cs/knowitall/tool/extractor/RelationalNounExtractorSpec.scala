package edu.washington.cs.knowitall.tool.extractor

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import edu.washington.cs.knowitall.tool.chunk.OpenNlpChunker
import edu.washington.cs.knowitall.tool.stem.MorphaStemmer

@RunWith(classOf[JUnitRunner])
class RelationalNounExtractorSpec extends Specification {
  val chunker = new OpenNlpChunker
  
  "relational noun extactor" should {
    val relnoun = new RelationalNounExtractor()
    
    val example = "United States president Barack Obama gave a speech today."
    val chunked = chunker.chunk(example)
    val lemmatized = chunked.map(MorphaStemmer.instance.lemmatizeToken)
      
    val extrs = relnoun(lemmatized)
    extrs.size == 1
    extrs.head.toString == "(Barack Obama; [is] president [of]; United States)"
  }
}