package edu.washington.cs.knowitall
package tool.extractor

import com.google.common.base.{ Function => GuavaFunction }
import scala.collection.JavaConverters._
import regex.RegularExpression
import regex.Match
import java.util.regex.Pattern
import tool.stem.Lemmatized
import tool.chunk.ChunkedToken
import edu.washington.cs.knowitall.regex.Expression
import edu.washington.cs.knowitall.logic.LogicExpression
import edu.washington.cs.knowitall.logic.{ Expression => LExpression }
import edu.washington.cs.knowitall.logic.ArgFactory
import com.google.common.base.Predicate
import PatternExtractor.Token

object PatternExtractor {
  type Token = Lemmatized[ChunkedToken]
  
  implicit def guavaFromFunction[A, B](f: A=>B) = new GuavaFunction[A, B] {
    override def apply(a: A) = f(a)
  }
  
  implicit def logicArgFromFunction[T](f: T=>Boolean) = new LExpression.Arg[T] {
    override def apply(token: T) = f(token)
  }

  def compile(pattern: String) = 
    RegularExpression.compile(pattern, (expression: String) => {
      val valuePattern = Pattern.compile("([\"'])(.*)\\1")
      
      val baseExpr = new Expression.BaseExpression[Token](expression) {
        val deserializeToken: String=>(Token=>Boolean) = (argument: String) => {
          val Array(base, value) = argument.split("=")
            
          val matcher = valuePattern.matcher(value)
          if (!matcher.matches()) {
            throw new IllegalArgumentException("Value not enclosed in quote (\") or ('): " + argument)
          }
            
          val string = matcher.group(2)
            
          base match {
            case "string" => new Expressions.StringExpression(string)
            case "lemma" => new Expressions.LemmaExpression(string)
            case "pos" => new Expressions.PostagExpression(string)
            case "chunk" => new Expressions.ChunkExpression(string)
          }
        }
        
        val logic: LogicExpression[Token] = 
          LogicExpression.compile(expression, deserializeToken andThen logicArgFromFunction[Token])
        
        override def apply(token: Token): Boolean = logic.apply(token)
      }
      
      baseExpr: Expression.BaseExpression[Token]
    })
}

abstract class BinaryPatternExtractor[B](val expression: RegularExpression[Token]) extends Extractor[Seq[Token], B]{
  def this(pattern: String) = this(PatternExtractor.compile(pattern))

  def apply(tokens: Seq[Token]): Iterable[B] = {
    val matches = expression.findAll(tokens.asJava);
    
    for (
      m <- matches.asScala;
      val extraction = buildExtraction(tokens, m);
      if !filterExtraction(extraction)
    ) yield extraction
  }

  protected def filterExtraction(extraction: B): Boolean = false

  protected def buildExtraction(tokens: Seq[Token], m: Match[Token]): B
}