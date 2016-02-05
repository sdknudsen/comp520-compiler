let cHeader =
"#include <stdlib.h>
#include <stdio.h>
#include <string.h>
 
char *rev(char *s) {
  int len = strlen(s);
  char *rev_s = malloc(sizeof(char) * (len + 1));
  size_t i;
  for (i = 0; i < len; ++i) {
    rev_s[len - i - 1] = s[i];
  }
  return rev_s;
}

char *concat(char *s1, char *s2) {
  int len1 = strlen(s1);
  int len2 = strlen(s2);
  char *new_s = malloc(sizeof(char) * (len1 + len2 + 1));
	      
  size_t k = 0;
  size_t i;
  for (i = 0; i < len1; ++i) {
    new_s[k++] = s1[i];
  }
  for (i = 0; i <= len2; ++i) {
    new_s[k++] = s2[i];
  }
  return new_s;
}
	     
int main(void) {
"

let cTail = "return 0;
}"
