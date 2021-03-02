package app.common

import hydro.common.GuavaReplacement.Splitter

object CaseFormats {
  def tokenize(s: String): Seq[String] = {
    Splitter.on('_').split(s)
  }
}