// Copyright 2013 Stefan Widgren and Maria Noremark,
// National Veterinary Institute, Sweden
//
// Licensed under the EUPL, Version 1.1 or - as soon they
// will be approved by the European Commission - subsequent
// versions of the EUPL (the "Licence");
// You may not use this work except in compliance with the
// Licence.
// You may obtain a copy of the Licence at:
//
// http://ec.europa.eu/idabc/eupl
//
// Unless required by applicable law or agreed to in
// writing, software distributed under the Licence is
// distributed on an "AS IS" basis,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
// express or implied.
// See the Licence for the specific language governing
// permissions and limitations under the Licence.

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
			       SEXP inBegin,
			       SEXP inEnd,
			       SEXP outBegin,
			       SEXP outEnd,
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

// Help class to keep track of visited nodes.
class VisitedNodes
{
public:
  VisitedNodes(size_t numberOfIdentifiers)
    : numberOfVisitedNodes(0),
      visitedNodes(numberOfIdentifiers)
  {}

  int N(void) const {return numberOfVisitedNodes;}
  void Update(int node, int tBegin, int tEnd, bool ingoing);
  bool Visit(int node, int tBegin, int tEnd, bool ingoing);

private:
  int numberOfVisitedNodes;
  std::vector<std::pair<bool, int> > visitedNodes;
};

typedef std::vector<Contact> Contacts;

#endif
