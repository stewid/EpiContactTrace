#include <utility>
#include <vector>
#include "trace.h"

using namespace Rcpp;
using namespace std;

// Help method to sort contacts by julian time
bool
compareT(pair<int, int> const& t1,
	 pair<int, int> const& t2)
{
  return t1.first < t2.first;
}

// Lookup of ingoing and outgoing conatcts
typedef pair<vector<map<int, Contacts> >, vector<map<int, Contacts> > > ContactsLookup;

ContactsLookup
buildContactsLookup(IntegerVector const& srcVec,
		    IntegerVector const& dstVec,
		    IntegerVector const& tVec,
		    int numberOfIdentifiers)
{
  vector<map<int, Contacts> > ingoing(numberOfIdentifiers);   // Lookup for ingoing contacts
  vector<map<int, Contacts> > outgoing(numberOfIdentifiers);  // Lookup for outfoing contacts
  vector<pair<int, int> > rowid; // first: julian time, second: original rowid

  // The contacts must be sorted by t.
  int n = tVec.size();
  rowid.reserve(n);
  for(int i=0;i<n;++i)
    {
      rowid.push_back(make_pair(tVec[i], i));
    }

  sort(rowid.begin(), rowid.end(), compareT);

  for(int i=0;i<n;++i)
    {
      int j = rowid[i].second;

      // Decrement with one since std::vector is zero-based
      int zb_src = srcVec[j] - 1;
      int zb_dst = dstVec[j] - 1;

      ingoing[zb_dst][zb_src].push_back(Contact(j, zb_src, tVec[j]));
      outgoing[zb_src][zb_dst].push_back(Contact(j, zb_dst, tVec[j]));
    }

  return make_pair(ingoing, outgoing);
}

void
traceContacts(const vector<map<int, Contacts> >& data,
	      int node,
	      int tBegin,
	      int tEnd,
	      set<int> visitedNodes,
	      int distance,
	      bool ingoing,
	      vector<int>& resultRowid,
	      vector<int>& resultDistance)
{
  visitedNodes.insert(node);

  for(map<int, Contacts>::const_iterator it = data[node].begin(),
	end = data[node].end();
      it != end;
      ++it)
    {
      // We are not interested in going in loops or backwards in the search path
      if(visitedNodes.find(it->first) == visitedNodes.end())
	{
	  // We are only interested in contacts within the specified time period,
	  // so first check the lower bound, tBegin
	  Contacts::const_iterator t_begin = lower_bound(it->second.begin(),
							 it->second.end(),
							 tBegin,
							 CompareContact());

	  if(t_begin != it->second.end() && t_begin->t_ <= tEnd)
	    {
	      // and then the upper bound, tEnd.
	      Contacts::const_iterator t_end = upper_bound(t_begin,
							   it->second.end(),
							   tEnd,
							   CompareContact());

	      for(Contacts::const_iterator iit=t_begin;
		  iit!=t_end;
		  ++iit)
		{
		  // Increment with one since R vector is one-based.
		  resultRowid.push_back(iit->rowid_ + 1);

		  resultDistance.push_back(distance);
		}

	      if(ingoing)
		{
		  traceContacts(data,
				 it->first,
				 tBegin,
				 (t_end-1)->t_,
				 visitedNodes,
				 distance + 1,
				 ingoing,
				 resultRowid,
				 resultDistance);
		}
	      else
		{
		  traceContacts(data,
				 it->first,
				 t_begin->t_,
				 tEnd,
				 visitedNodes,
				 distance + 1,
				 ingoing,
				 resultRowid,
				 resultDistance);
		}
	    }
	}
    }
}

SEXP traceContacts(SEXP src,
		   SEXP dst,
		   SEXP t,
		   SEXP root,
		   SEXP inBegin,
		   SEXP inEnd,
		   SEXP outBegin,
		   SEXP outEnd,
		   SEXP numberOfIdentifiers)
{
    IntegerVector srcVec(src);
    IntegerVector dstVec(dst);
    IntegerVector tVec(t);
    IntegerVector rootVec(root);
    IntegerVector inBeginVec(inBegin);
    IntegerVector inEndVec(inEnd);
    IntegerVector outBeginVec(outBegin);
    IntegerVector outEndVec(outEnd);

    ContactsLookup lookup = buildContactsLookup(srcVec,
						dstVec,
						tVec,
						as<int>(numberOfIdentifiers));

    List result;
    vector<int> resultRowid;
    vector<int> resultDistance;

    for(int i=0, end=rootVec.size(); i<end; ++i)
      {
	resultRowid.clear();
	resultDistance.clear();

	traceContacts(lookup.first,
		      rootVec[i] - 1,
		      inBeginVec[i],
		      inEndVec[i],
		      set<int>(),
		      1,
		      true,
		      resultRowid,
		      resultDistance);

	result.push_back(resultRowid);
	result.push_back(resultDistance);

	resultRowid.clear();
	resultDistance.clear();

	traceContacts(lookup.second,
		      rootVec[i] - 1,
		      outBeginVec[i],
		      outEndVec[i],
		      set<int>(),
		      1,
		      false,
		      resultRowid,
		      resultDistance);

	result.push_back(resultRowid);
	result.push_back(resultDistance);
      }

    return result;
}

int
degree(const vector<map<int, Contacts> >& data,
       int node,
       int tBegin,
       int tEnd)
{
  int result = 0;

  for(map<int, Contacts>::const_iterator it = data[node].begin();
      it != data[node].end();
      ++it)
    {
      // We are not interested in going in loops.
      if(node != it->first)
	{
	  // We are only interested in contacts within the specified time period,
	  // so first check the lower bound, tBegin
	  Contacts::const_iterator t_begin = lower_bound(it->second.begin(),
							 it->second.end(),
							 tBegin,
							 CompareContact());

	  if(t_begin != it->second.end() && t_begin->t_ <= tEnd)
	    {
	      ++result;
	    }
	}
    }

  return result;
}

void
contactChain(const vector<map<int, Contacts> >& data,
	     int node,
	     int tBegin,
	     int tEnd,
	     set<int> visitedNodes,
	     bool ingoing,
	     set<int>& resultNodes)
{
  visitedNodes.insert(node);

  for(map<int, Contacts>::const_iterator it = data[node].begin(),
	end = data[node].end();
      it != end;
      ++it)
    {
      // We are not interested in going in loops or backwards in the search path
      if(visitedNodes.find(it->first) == visitedNodes.end())
	{
	  // We are only interested in contacts within the specified time period,
	  // so first check the lower bound, tBegin
	  Contacts::const_iterator t_begin = lower_bound(it->second.begin(),
							 it->second.end(),
							 tBegin,
							 CompareContact());

	  if(t_begin != it->second.end() && t_begin->t_ <= tEnd)
	    {
	      resultNodes.insert(it->first);

	      if(ingoing)
		{
		  // and then the upper bound, tEnd.
		  Contacts::const_iterator t_end = upper_bound(t_begin,
							       it->second.end(),
							       tEnd,
							       CompareContact());

		  contactChain(data,
			       it->first,
			       tBegin,
			       (t_end-1)->t_,
			       visitedNodes,
			       ingoing,
			       resultNodes);
		}
	      else
		{
		  contactChain(data,
			       it->first,
			       t_begin->t_,
			       tEnd,
			       visitedNodes,
			       ingoing,
			       resultNodes);
		}
	    }
	}
    }
}

SEXP networkSummary(SEXP src,
		    SEXP dst,
		    SEXP t,
		    SEXP root,
		    SEXP tBegin,
		    SEXP tEnd,
		    SEXP numberOfIdentifiers)
{
    IntegerVector srcVec(src);
    IntegerVector dstVec(dst);
    IntegerVector tVec(t);
    IntegerVector rootVec(root);
    IntegerVector tBeginVec(tBegin);
    IntegerVector tEndVec(tEnd);

    ContactsLookup lookup = buildContactsLookup(srcVec,
						dstVec,
						tVec,
						as<int>(numberOfIdentifiers));

    vector<int> ingoingContactChain;
    vector<int> outgoingContactChain;
    vector<int> inDegree;
    vector<int> outDegree;
    set<int> resultNodes;

    for(int i=0, end=rootVec.size(); i<end; ++i)
      {
	resultNodes.clear();
	contactChain(lookup.first,
		     rootVec[i] - 1,
		     tBeginVec[i],
		     tEndVec[i],
		     set<int>(),
		     true,
		     resultNodes);

	ingoingContactChain.push_back(resultNodes.size());

	resultNodes.clear();
	contactChain(lookup.second,
		     rootVec[i] - 1,
		     tBeginVec[i],
		     tEndVec[i],
		     set<int>(),
		     false,
		     resultNodes);

	outgoingContactChain.push_back(resultNodes.size());

	inDegree.push_back(degree(lookup.first,
				  rootVec[i] - 1,
				  tBeginVec[i],
				  tEndVec[i]));

	outDegree.push_back(degree(lookup.second,
				   rootVec[i] - 1,
				   tBeginVec[i],
				   tEndVec[i]));
      }

    return List::create(_["inDegree"] = inDegree,
			_["outDegree"] = outDegree,
			_["ingoingContactChain"] = ingoingContactChain,
			_["outgoingContactChain"] = outgoingContactChain);
}

