// Copyright 2013-2024 Stefan Widgren and Maria Noremark,
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

#define R_NO_REMAP
#define STRICT_R_HEADERS

#include <algorithm>
#include <map>
#include <set>
#include <utility>
#include <vector>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

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

class CompareContact {
public:
    bool operator()(const Contact& c, int t) {
        return c.t_ < t;
    }

    bool operator()(int t, const Contact& c) {
        return t < c.t_;
    }
};

// Help class to keep track of visited nodes.
class VisitedNodes {
public:
    VisitedNodes(size_t numberOfIdentifiers)
        : numberOfVisitedNodes(0),
          visitedNodes(numberOfIdentifiers)
        {}

    int N(void) const {
        return numberOfVisitedNodes;
    }

    void Update(int node, int tBegin, int tEnd, bool ingoing) {
        if (visitedNodes[node].first) {
            if (ingoing) {
                if (tEnd > visitedNodes[node].second) {
                    visitedNodes[node].second = tEnd;
                }
            } else if (tBegin < visitedNodes[node].second) {
                visitedNodes[node].second = tBegin;
            }
        } else {
            visitedNodes[node].first = true;
            numberOfVisitedNodes++;
            if (ingoing)
                visitedNodes[node].second = tEnd;
            else
                visitedNodes[node].second = tBegin;
        }
    }

    bool Visit(int node, int tBegin, int tEnd, bool ingoing) {
        if (visitedNodes[node].first) {
            if (ingoing) {
                if (tEnd <= visitedNodes[node].second) {
                    return false;
                }
            } else if (tBegin >= visitedNodes[node].second) {
                return false;
            }
        }

        return true;
    }

private:
    int numberOfVisitedNodes;
    std::vector<std::pair<bool, int> > visitedNodes;
};

typedef std::vector<Contact> Contacts;

static int
check_arguments(const SEXP src,
                const SEXP dst,
                const SEXP t,
                const SEXP root,
                const SEXP inBegin,
                const SEXP inEnd,
                const SEXP outBegin,
                const SEXP outEnd,
                const SEXP numberOfIdentifiers)
{
    if (R_NilValue == root
        || R_NilValue == inBegin
        || R_NilValue == inEnd
        || R_NilValue == outBegin
        || R_NilValue == outEnd
        || R_NilValue == numberOfIdentifiers
        || INTSXP != TYPEOF(root)
        || INTSXP != TYPEOF(inBegin)
        || INTSXP != TYPEOF(inEnd)
        || INTSXP != TYPEOF(outBegin)
        || INTSXP != TYPEOF(outEnd)
        || INTSXP != TYPEOF(numberOfIdentifiers)
        || 1 != LENGTH(numberOfIdentifiers))
        return 1;
    return 0;
}

// Help method to sort contacts by julian time
bool
compareT(std::pair<int, int> const& t1, std::pair<int, int> const& t2)
{
    return t1.first < t2.first;
}

// Lookup of ingoing and outgoing conatcts
typedef std::pair<std::vector<std::map<int, Contacts> >,
                  std::vector<std::map<int, Contacts> > > ContactsLookup;

static ContactsLookup
buildContactsLookup(const int *src,
		    const int *dst,
		    const int *t,
                    const size_t len,
		    const size_t numberOfIdentifiers)
{
    // Lookup for ingoing contacts
    std::vector<std::map<int, Contacts> > ingoing(numberOfIdentifiers);

    // Lookup for outfoing contacts
    std::vector<std::map<int, Contacts> > outgoing(numberOfIdentifiers);

    // first: julian time, second: original rowid
    std::vector<std::pair<int, int> > rowid;

    if (NULL == src || NULL == dst || NULL == t)
        Rf_error("Unable to build contacts lookup");

    // The contacts must be sorted by t.
    rowid.reserve(len);
    for (size_t i=0;i<len;++i) {
        rowid.push_back(std::make_pair(t[i], i));
    }

    sort(rowid.begin(), rowid.end(), compareT);

    for (size_t i=0;i<len;++i) {
        int j = rowid[i].second;

        // Decrement with one since std::vector is zero-based
        int zb_src = src[j] - 1;
        int zb_dst = dst[j] - 1;

        ingoing[zb_dst][zb_src].push_back(Contact(j, zb_src, t[j]));
        outgoing[zb_src][zb_dst].push_back(Contact(j, zb_dst, t[j]));
    }

    return make_pair(ingoing, outgoing);
}

static void
doShortestPaths(const std::vector<std::map<int, Contacts> >& data,
                const int node,
                const int tBegin,
                const int tEnd,
                std::set<int> visitedNodes,
                const int distance,
                const bool ingoing,
                std::map<int, std::pair<int, int> >& result)
{
    visitedNodes.insert(node);

    for (std::map<int, Contacts>::const_iterator it = data[node].begin(),
            end = data[node].end(); it != end; ++it)
    {
        // We are not interested in going in loops or backwards in the
        // search path
        if (visitedNodes.find(it->first) == visitedNodes.end()) {
            // We are only interested in contacts within the specified
            // time period, so first check the lower bound, tBegin
            Contacts::const_iterator t_begin =
                std::lower_bound(it->second.begin(),
                                 it->second.end(),
                                 tBegin,
                                 CompareContact());

            if (t_begin != it->second.end() && t_begin->t_ <= tEnd) {
                int t0, t1;

                std::map<int, std::pair<int, int> >::iterator distance_it =
                    result.find(it->first);
                if (distance_it == result.end()) {
                    result[it->first].first = distance;

                    // Increment with one since R vector is one-based.
                    result[it->first].second = t_begin->rowid_ + 1;
                }  else if (distance < distance_it->second.first) {
                    distance_it->second.first = distance;

                    // Increment with one since R vector is one-based.
                    distance_it->second.second = t_begin->rowid_ + 1;
                }

                if (ingoing) {
                    // and then the upper bound, tEnd.
                    Contacts::const_iterator t_end =
                        std::upper_bound(t_begin,
                                         it->second.end(),
                                         tEnd,
                                         CompareContact());

                    t0 = tBegin;
                    t1 = (t_end-1)->t_;
                } else {
                    t0 = t_begin->t_;
                    t1 = tEnd;
                }

                doShortestPaths(data,
                                it->first,
                                t0,
                                t1,
                                visitedNodes,
                                distance + 1,
                                ingoing,
                                result);
            }
        }
    }
}

extern "C"
SEXP shortestPaths(const SEXP src,
		   const SEXP dst,
		   const SEXP t,
		   const SEXP root,
		   const SEXP inBegin,
		   const SEXP inEnd,
		   const SEXP outBegin,
		   const SEXP outEnd,
		   const SEXP numberOfIdentifiers)
{
    if (check_arguments(src, dst, t, root, inBegin, inEnd,
                       outBegin, outEnd, numberOfIdentifiers))
        Rf_error("Unable to calculate shortest paths");

    ContactsLookup lookup =
        buildContactsLookup(INTEGER(src),
                            INTEGER(dst),
                            INTEGER(t),
                            LENGTH(t),
                            INTEGER(numberOfIdentifiers)[0]);

    size_t len = LENGTH(root);
    std::vector<int> inRowid;
    std::vector<int> outRowid;
    std::vector<int> inDistance;
    std::vector<int> outDistance;
    std::vector<int> inIndex;
    std::vector<int> outIndex;
    for (size_t i=0; i<len; ++i) {
        // Key: node, Value: first: distance, second: original rowid
        std::map<int, std::pair<int, int> > ingoingShortestPaths;

        // Key: node, Value: first: distance, second: original rowid
        std::map<int, std::pair<int, int> > outgoingShortestPaths;

        doShortestPaths(lookup.first,
                        INTEGER(root)[i] - 1,
                        INTEGER(inBegin)[i],
                        INTEGER(inEnd)[i],
                        std::set<int>(),
                        1,
                        true,
                        ingoingShortestPaths);

        for (std::map<int, std::pair<int, int> >::const_iterator it =
                ingoingShortestPaths.begin();
            it!=ingoingShortestPaths.end(); ++it)
        {
            inDistance.push_back(it->second.first);
            inRowid.push_back(it->second.second);
            inIndex.push_back(i+1);
        }

        doShortestPaths(lookup.second,
                        INTEGER(root)[i] - 1,
                        INTEGER(outBegin)[i],
                        INTEGER(outEnd)[i],
                        std::set<int>(),
                        1,
                        false,
                        outgoingShortestPaths);

        for (std::map<int, std::pair<int, int> >::const_iterator it =
                outgoingShortestPaths.begin();
            it!=outgoingShortestPaths.end(); ++it)
        {
            outDistance.push_back(it->second.first);
            outRowid.push_back(it->second.second);
            outIndex.push_back(i+1);
        }
    }

    SEXP result, names, vec;
    PROTECT(result = Rf_allocVector(VECSXP, 6));
    PROTECT(names = Rf_allocVector(STRSXP, 6));

    SET_VECTOR_ELT(result, 0, vec = Rf_allocVector(INTSXP, inDistance.size()));
    for (size_t i = 0; i < inDistance.size(); ++i)
        INTEGER(vec)[i] = inDistance[i];
    SET_STRING_ELT(names,  0, Rf_mkChar("inDistance"));

    SET_VECTOR_ELT(result, 1, vec = Rf_allocVector(INTSXP, inRowid.size()));
    for (size_t i = 0; i < inRowid.size(); ++i)
        INTEGER(vec)[i] = inRowid[i];
    SET_STRING_ELT(names,  1, Rf_mkChar("inRowid"));

    SET_VECTOR_ELT(result, 2, vec = Rf_allocVector(INTSXP, inIndex.size()));
    for (size_t i = 0; i < inIndex.size(); ++i)
        INTEGER(vec)[i] = inIndex[i];
    SET_STRING_ELT(names,  2, Rf_mkChar("inIndex"));

    SET_VECTOR_ELT(result, 3, vec = Rf_allocVector(INTSXP, outDistance.size()));
    for (size_t i = 0; i < outDistance.size(); ++i)
        INTEGER(vec)[i] = outDistance[i];
    SET_STRING_ELT(names,  3, Rf_mkChar("outDistance"));

    SET_VECTOR_ELT(result, 4, vec = Rf_allocVector(INTSXP, outRowid.size()));
    for (size_t i = 0; i < outRowid.size(); ++i)
        INTEGER(vec)[i] = outRowid[i];
    SET_STRING_ELT(names,  4, Rf_mkChar("outRowid"));

    SET_VECTOR_ELT(result, 5, vec = Rf_allocVector(INTSXP, outIndex.size()));
    for (size_t i = 0; i < outIndex.size(); ++i)
        INTEGER(vec)[i] = outIndex[i];
    SET_STRING_ELT(names,  5, Rf_mkChar("outIndex"));

    Rf_setAttrib(result, R_NamesSymbol, names);
    UNPROTECT(2);

    return result;
}

static void
doTraceContacts(const std::vector<std::map<int, Contacts> >& data,
                const int node,
                const int tBegin,
                const int tEnd,
                std::set<int> visitedNodes,
                const int distance,
                const bool ingoing,
                std::vector<int>& resultRowid,
                std::vector<int>& resultDistance,
                const int maxDistance)
{
    visitedNodes.insert(node);

    for (std::map<int, Contacts>::const_iterator it = data[node].begin(),
            end = data[node].end(); it != end; ++it)
    {
        // We are not interested in going in loops or backwards in the
        // search path
        if (visitedNodes.find(it->first) == visitedNodes.end()) {
            // We are only interested in contacts within the specified
            // time period, so first check the lower bound, tBegin
            Contacts::const_iterator t_begin =
                std::lower_bound(it->second.begin(),
                                 it->second.end(),
                                 tBegin,
                                 CompareContact());

            if (t_begin != it->second.end() && t_begin->t_ <= tEnd) {
                int t0, t1;

                // and then the upper bound, tEnd.
                Contacts::const_iterator t_end =
                    std::upper_bound(t_begin,
                                     it->second.end(),
                                     tEnd,
                                     CompareContact());

                for (Contacts::const_iterator iit=t_begin; iit!=t_end; ++iit) {
                    // Increment with one since R vector is one-based.
                    resultRowid.push_back(iit->rowid_ + 1);

                    resultDistance.push_back(distance);
                }

                if (maxDistance > 0 && distance >= maxDistance)
                    continue;

                if (ingoing) {
                    t0 = tBegin;
                    t1 = (t_end-1)->t_;
                } else {
                    t0 = t_begin->t_;
                    t1 = tEnd;
                }

                doTraceContacts(data,
                                it->first,
                                t0,
                                t1,
                                visitedNodes,
                                distance + 1,
                                ingoing,
                                resultRowid,
                                resultDistance,
                                maxDistance);
            }
        }
    }
}

extern "C"
SEXP traceContacts(const SEXP src,
		   const SEXP dst,
		   const SEXP t,
		   const SEXP root,
		   const SEXP inBegin,
		   const SEXP inEnd,
		   const SEXP outBegin,
		   const SEXP outEnd,
		   const SEXP numberOfIdentifiers,
                   const SEXP maxDistance)
{
    if (check_arguments(src, dst, t, root, inBegin, inEnd, outBegin, outEnd,
                        numberOfIdentifiers)) {
        Rf_error("Unable to trace contacts");
    }

    ContactsLookup lookup =
        buildContactsLookup(INTEGER(src),
                            INTEGER(dst),
                            INTEGER(t),
                            LENGTH(t),
                            INTEGER(numberOfIdentifiers)[0]);

    SEXP result, vec;
    std::vector<int> resultRowid;
    std::vector<int> resultDistance;

    PROTECT(result = Rf_allocVector(VECSXP, 4 * LENGTH(root)));
    for (size_t i = 0, end = LENGTH(root); i < end; ++i) {
        resultRowid.clear();
        resultDistance.clear();

        doTraceContacts(lookup.first,
                        INTEGER(root)[i] - 1,
                        INTEGER(inBegin)[i],
                        INTEGER(inEnd)[i],
                        std::set<int>(),
                        1,
                        true,
                        resultRowid,
                        resultDistance,
                        INTEGER(maxDistance)[0]);

        SET_VECTOR_ELT(result, 4 * i, vec = Rf_allocVector(INTSXP, resultRowid.size()));
        for (size_t j = 0; j < resultRowid.size(); ++j)
            INTEGER(vec)[j] = resultRowid[j];

        SET_VECTOR_ELT(result, 4 * i + 1, vec = Rf_allocVector(INTSXP, resultDistance.size()));
        for (size_t j = 0; j < resultDistance.size(); ++j)
            INTEGER(vec)[j] = resultDistance[j];

        resultRowid.clear();
        resultDistance.clear();

        doTraceContacts(lookup.second,
                        INTEGER(root)[i] - 1,
                        INTEGER(outBegin)[i],
                        INTEGER(outEnd)[i],
                        std::set<int>(),
                        1,
                        false,
                        resultRowid,
                        resultDistance,
                        INTEGER(maxDistance)[0]);

        SET_VECTOR_ELT(result, 4 * i + 2, vec = Rf_allocVector(INTSXP, resultRowid.size()));
        for (size_t j = 0; j < resultRowid.size(); ++j)
            INTEGER(vec)[j] = resultRowid[j];

        SET_VECTOR_ELT(result, 4 * i + 3, vec = Rf_allocVector(INTSXP, resultDistance.size()));
        for (size_t j = 0; j < resultDistance.size(); ++j)
            INTEGER(vec)[j] = resultDistance[j];
    }

    UNPROTECT(1);

    return result;
}

static int
degree(const std::vector<std::map<int, Contacts> >& data,
       const int node,
       const int tBegin,
       const int tEnd)
{
    int result = 0;

    for (std::map<int, Contacts>::const_iterator it = data[node].begin();
        it != data[node].end();
        ++it)
    {
        // We are not interested in going in loops.
        if (node != it->first) {
            // We are only interested in contacts within the specified
            // time period, so first check the lower bound, tBegin
            Contacts::const_iterator t_begin =
                std::lower_bound(it->second.begin(),
                                 it->second.end(),
                                 tBegin,
                                 CompareContact());

            if (t_begin != it->second.end() && t_begin->t_ <= tEnd) {
                ++result;
            }
        }
    }

    return result;
}

static void
contactChain(const std::vector<std::map<int, Contacts> >& data,
	     const int node,
	     const int tBegin,
	     const int tEnd,
	     VisitedNodes& visitedNodes,
	     const bool ingoing)
{
    visitedNodes.Update(node, tBegin, tEnd, ingoing);

    for (std::map<int, Contacts>::const_iterator it = data[node].begin(),
            end = data[node].end(); it != end; ++it)
    {
        if (visitedNodes.Visit(it->first, tBegin, tEnd, ingoing)) {
            // We are only interested in contacts within the specified
            // time period, so first check the lower bound, tBegin
            Contacts::const_iterator t_begin =
                std::lower_bound(it->second.begin(),
                                 it->second.end(),
                                 tBegin,
                                 CompareContact());

            if (t_begin != it->second.end() && t_begin->t_ <= tEnd) {
                int t0, t1;

                if (ingoing) {
                    // and then the upper bound, tEnd.
                    Contacts::const_iterator t_end =
                        std::upper_bound(t_begin,
                                         it->second.end(),
                                         tEnd,
                                         CompareContact());

                    t0 = tBegin;
                    t1 = (t_end-1)->t_;
                } else {
                    t0 = t_begin->t_;
                    t1 = tEnd;
                }

                contactChain(data, it->first, t0, t1, visitedNodes, ingoing);
            }
        }
    }
}

extern "C"
SEXP networkSummary(const SEXP src,
		    const SEXP dst,
		    const SEXP t,
		    const SEXP root,
		    const SEXP inBegin,
		    const SEXP inEnd,
		    const SEXP outBegin,
		    const SEXP outEnd,
		    const SEXP numberOfIdentifiers)
{
    if (check_arguments(src, dst, t, root, inBegin, inEnd,
                       outBegin, outEnd, numberOfIdentifiers))
        Rf_error("Unable to calculate network summary");

    ContactsLookup lookup =
        buildContactsLookup(INTEGER(src),
                            INTEGER(dst),
                            INTEGER(t),
                            LENGTH(t),
                            INTEGER(numberOfIdentifiers)[0]);

    std::vector<int> ingoingContactChain;
    std::vector<int> outgoingContactChain;
    std::vector<int> inDegree;
    std::vector<int> outDegree;

    for (size_t i=0, end=LENGTH(root); i<end; ++i) {
        VisitedNodes visitedNodesIngoing(INTEGER(numberOfIdentifiers)[0]);
        VisitedNodes visitedNodesOutgoing(INTEGER(numberOfIdentifiers)[0]);

        contactChain(lookup.first,
                     INTEGER(root)[i] - 1,
                     INTEGER(inBegin)[i],
                     INTEGER(inEnd)[i],
                     visitedNodesIngoing,
                     true);

        contactChain(lookup.second,
                     INTEGER(root)[i] - 1,
                     INTEGER(outBegin)[i],
                     INTEGER(outEnd)[i],
                     visitedNodesOutgoing,
                     false);

        ingoingContactChain.push_back(visitedNodesIngoing.N() - 1);
        outgoingContactChain.push_back(visitedNodesOutgoing.N() - 1);

        inDegree.push_back(degree(lookup.first,
                                  INTEGER(root)[i] - 1,
                                  INTEGER(inBegin)[i],
                                  INTEGER(inEnd)[i]));

        outDegree.push_back(degree(lookup.second,
                                   INTEGER(root)[i] - 1,
                                   INTEGER(outBegin)[i],
                                   INTEGER(outEnd)[i]));
    }

    SEXP result, names, vec;
    PROTECT(result = Rf_allocVector(VECSXP, 4));
    PROTECT(names = Rf_allocVector(STRSXP, 4));

    SET_VECTOR_ELT(result, 0, vec = Rf_allocVector(INTSXP, inDegree.size()));
    for (size_t i = 0; i < inDegree.size(); ++i)
        INTEGER(vec)[i] = inDegree[i];
    SET_STRING_ELT(names,  0, Rf_mkChar("inDegree"));

    SET_VECTOR_ELT(result, 1, vec = Rf_allocVector(INTSXP, outDegree.size()));
    for (size_t i = 0; i < outDegree.size(); ++i)
        INTEGER(vec)[i] = outDegree[i];
    SET_STRING_ELT(names,  1, Rf_mkChar("outDegree"));

    SET_VECTOR_ELT(result, 2, vec = Rf_allocVector(INTSXP, ingoingContactChain.size()));
    for (size_t i = 0; i < ingoingContactChain.size(); ++i)
        INTEGER(vec)[i] = ingoingContactChain[i];
    SET_STRING_ELT(names,  2, Rf_mkChar("ingoingContactChain"));

    SET_VECTOR_ELT(result, 3, vec = Rf_allocVector(INTSXP, outgoingContactChain.size()));
    for (size_t i = 0; i < outgoingContactChain.size(); ++i)
        INTEGER(vec)[i] = outgoingContactChain[i];
    SET_STRING_ELT(names,  3, Rf_mkChar("outgoingContactChain"));

    Rf_setAttrib(result, R_NamesSymbol, names);
    UNPROTECT(2);

    return result;
}

static const R_CallMethodDef callMethods[] =
{
    {"networkSummary", (DL_FUNC) &networkSummary, 9},
    {"shortestPaths", (DL_FUNC) &shortestPaths, 9},
    {"traceContacts", (DL_FUNC) &traceContacts, 10},
    {NULL, NULL, 0}
};

/** Register routines to R
 * @param info Information about the DLL being loaded
 */
void attribute_visible
R_init_EpiContactTrace(DllInfo *info)
{
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
    R_forceSymbols(info, TRUE);
}
