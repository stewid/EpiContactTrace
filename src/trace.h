#ifndef _TRACE_H
#define _TRACE_H

#include <Rcpp.h>

/*
 * note : RcppExport is an alias to `extern "C"` defined by Rcpp.
 *
 * It gives C calling convention to the rcpp_hello_world function so that
 * it can be called from .Call in R. Otherwise, the C++ compiler mangles the
 * name of the function and .Call can't find it.
 *
 * It is only useful to use RcppExport when the function is intended to be called
 * by .Call. See the thread http://thread.gmane.org/gmane.comp.lang.r.rcpp/649/focus=672
 * on Rcpp-devel for a misuse of RcppExport
 */

RcppExport SEXP traceContacts(SEXP src,
			      SEXP dst,
			      SEXP t,
			      SEXP root,
			      SEXP inBegin,
			      SEXP inEnd,
			      SEXP outBegin,
			      SEXP outEnd,
			      SEXP numberOfIdentifiers);

RcppExport SEXP networkSummary(SEXP src,
			       SEXP dst,
			       SEXP t,
			       SEXP root,
			       SEXP tBegin,
			       SEXP tEnd,
			       SEXP numberOfIdentifiers);

class Contact {
public:
  Contact(int rowid, int identifier, int t)
    : rowid_(rowid), identifier_(identifier), t_(t)
  {
  }

  int rowid_;
  int identifier_;
  int t_;
};

class CompareContact
{
public:
  bool operator()(const Contact& c, int t)
  {
    return c.t_ < t;
  }

  bool operator()(int t, const Contact& c)
  {
    return t < c.t_;
  }
};

typedef std::vector<Contact> Contacts;

#endif
