#include <stdio.h>
#include <stdlib.h>
#include "gpp_interpreter.tab.h"

extern int yyparse();   // Yacc'ın ürettiği parser fonksiyonu
extern FILE *yyin;      // Flex'in kullandığı giriş dosyası

int main(int argc, char **argv) {
    if (argc > 1) {
        // Giriş dosyasını aç
        yyin = fopen(argv[1], "r");
        if (!yyin) {
            perror("Error opening file");
            return 1;
        }
    } else {
        // Standart giriş
        printf("Enter your expressions:\n");
    }

    // Parser'ı çalıştır
    if (yyparse() == 0) {
        printf("Parsing completed successfully.\n");
    } else {
        printf("Parsing failed.\n");
    }

    return 0;
}
