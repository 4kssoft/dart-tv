// Copyright (c) 2021, the Dart project authors. Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

import 'package:analysis_server/src/protocol_server.dart';
import 'package:analysis_server/src/services/completion/filtering/fuzzy_matcher.dart';

/// Filters and scores [suggestions] according to how well they match the
/// [pattern]. Sorts [suggestions] by the score, relevance, and name.
List<CompletionSuggestion> fuzzyFilterSort({
  required String pattern,
  required List<CompletionSuggestion> suggestions,
}) {
  var matcher = FuzzyMatcher(pattern, matchStyle: MatchStyle.SYMBOL);

  double score(CompletionSuggestion suggestion) {
    var suggestionTextToMatch = suggestion.completion;

    if (suggestion.kind == CompletionSuggestionKind.NAMED_ARGUMENT) {
      var index = suggestionTextToMatch.indexOf(':');
      if (index != -1) {
        suggestionTextToMatch = suggestionTextToMatch.substring(0, index);
      }
    }

    return matcher.score(suggestionTextToMatch);
  }

  var scored = suggestions
      .map((e) => _FuzzyScoredSuggestion(e, score(e)))
      .where((e) => e.score > 0)
      .toList();

  scored.sort((a, b) {
    // Prefer what the user requested by typing.
    if (a.score > b.score) {
      return -1;
    } else if (a.score < b.score) {
      return 1;
    }

    // Then prefer what is more relevant in the context.
    if (a.suggestion.relevance != b.suggestion.relevance) {
      return b.suggestion.relevance - a.suggestion.relevance;
    }

    // Other things being equal, sort by name.
    return a.suggestion.completion.compareTo(b.suggestion.completion);
  });

  return scored.map((e) => e.suggestion).toList();
}

/// [CompletionSuggestion] scored using [FuzzyMatcher].
class _FuzzyScoredSuggestion {
  final CompletionSuggestion suggestion;
  final double score;

  _FuzzyScoredSuggestion(this.suggestion, this.score);
}
