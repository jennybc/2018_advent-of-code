#include <string>
#include <algorithm>
#include <Rcpp.h>

// [[Rcpp::export]]
std::string react_cpp(std::string s){
  int i = 0;

  bool r = FALSE;
  while (s.size() > 0 && i != s.size() - 1) {
    r = toupper(s[i]) == toupper(s[i + 1]) && s[i] != s[i + 1];
    if (r) {
      s.erase(i, 2);
      if (i > 0) --i;
    } else {
      ++i;
    }
  }

  return(s);
}
