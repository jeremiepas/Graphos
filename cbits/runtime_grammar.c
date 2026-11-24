/*
 * Thin C shim over tree-sitter's query API for
 * Graphos.Infrastructure.Extract.TreeSitter.RuntimeGrammar.
 *
 * Keeping the TSQueryMatch/TSQueryCapture struct layouts on the C side avoids
 * duplicating (and mis-guessing) tree-sitter's ABI in Haskell. The language
 * runtime itself is linked from the `tree-sitter` Haskell package.
 */
#include <stdint.h>
#include <stdlib.h>
#include <tree_sitter/api.h>

typedef struct {
  uint32_t capture_index;
  uint32_t start_byte;
  uint32_t end_byte;
  uint32_t start_row;
  uint32_t start_column;
  uint32_t end_row;
  uint32_t end_column;
} GraphosQueryCapture;

TSQuery *graphos_ts_query_new(const TSLanguage *language, const char *source,
                              uint32_t source_len, uint32_t *error_offset,
                              uint32_t *error_type) {
  TSQueryError error = TSQueryErrorNone;
  TSQuery *query = ts_query_new(language, source, source_len, error_offset,
                                &error);
  *error_type = (uint32_t)error;
  return query;
}

void graphos_ts_query_delete(TSQuery *query) {
  ts_query_delete(query);
}

GraphosQueryCapture *graphos_ts_query_captures(const TSNode *root,
                                               const TSQuery *query,
                                               uint32_t *count) {
  *count = 0;
  TSQueryCursor *cursor = ts_query_cursor_new();
  if (cursor == NULL) {
    return NULL;
  }

  ts_query_cursor_exec(cursor, query, *root);

  uint32_t capacity = 16;
  uint32_t n = 0;
  GraphosQueryCapture *captures =
      (GraphosQueryCapture *)malloc(capacity * sizeof(GraphosQueryCapture));
  if (captures == NULL) {
    ts_query_cursor_delete(cursor);
    return NULL;
  }

  TSQueryMatch match;
  while (ts_query_cursor_next_match(cursor, &match)) {
    for (uint16_t i = 0; i < match.capture_count; i++) {
      if (n == capacity) {
        capacity *= 2;
        GraphosQueryCapture *grown = (GraphosQueryCapture *)realloc(
            captures, capacity * sizeof(GraphosQueryCapture));
        if (grown == NULL) {
          free(captures);
          ts_query_cursor_delete(cursor);
          return NULL;
        }
        captures = grown;
      }
      TSNode node = match.captures[i].node;
      TSPoint start = ts_node_start_point(node);
      TSPoint end = ts_node_end_point(node);
      captures[n].capture_index = match.captures[i].index;
      captures[n].start_byte = ts_node_start_byte(node);
      captures[n].end_byte = ts_node_end_byte(node);
      captures[n].start_row = start.row;
      captures[n].start_column = start.column;
      captures[n].end_row = end.row;
      captures[n].end_column = end.column;
      n++;
    }
  }

  ts_query_cursor_delete(cursor);
  *count = n;
  return captures;
}

void graphos_ts_query_captures_free(GraphosQueryCapture *captures) {
  free(captures);
}
