#include <tree_sitter/parser.h>

enum TokenType {
  STRING_CONTENT
};

extern "C" {
  void * tree_sitter_perl_external_scanner_create() {
    return NULL;
  }

  void tree_sitter_perl_external_scanner_destroy(void *payload) {
  }

  unsigned tree_sitter_perl_external_scanner_serialize(
    void *payload,
    char *buffer
  ) {
    return 0;
  }

  void tree_sitter_perl_external_scanner_deserialize(
    void *payload,
    const char *buffer,
    unsigned length
  ) {
  }

  static void advance(TSLexer *lexer) { lexer->advance(lexer, false); }

  bool tree_sitter_perl_external_scanner_scan(
    void *payload,
    TSLexer *lexer,
    const bool *valid_symbols
  ) {
    return false;
  }
}
