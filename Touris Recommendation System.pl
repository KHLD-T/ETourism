%Prolog eval at the very end


% offerMean(X, Y) -> Transportation mean Y is used with offer X

offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 4), bus).

% offerAccommodation(X, Y) -> Accommodation Y is part of offer X

offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 4), hotel).

% customerPreferredActivity(X, Y, R) -> Y is the preferred activity kind wrt customer X with relevance R

customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding, 50).

% customerPreferredMean(X, Y, R) -> Y is the preferred transportaion mean wrt customer X with relevance R

customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).

% customerPreferredAccommodation(X, Y, R) -> Y is the preferred accommodation to customer X with relevance R

customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), cabin, 50).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel, 100).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), cabin, 79).

%possibleSubset(L,R)R is one of perm of subset of L
sub([], []).
sub([H|T], [H|NT]):-
                     sub(T, NT).
sub([_|T], NT):-
                     sub(T, NT).
possibleSubset([],[]).
possibleSubset([L|T],R):-
                     L\=activity(_),
                     sub([L|T],L1),
                     permutation(L1,R).
possibleSubset(activity(X),activity(R)):-
                     sub(X,L1),
                     permutation(L1,R).

%choosePref(Prefs,Chosen), should have every possible combination of preferences and gets the permutations of whatever inside activity after asking prolog for more possible solutions
choosePreferences(Prefs,ChosenPrefs):-
                     \+member(activity(_),Prefs),
                     possibleSubset(Prefs,ChosenPrefs).
choosePreferences([H|T],ChosenPrefs):-
                     member(activity(_),[H|T]),
                     H\=activity(_),      %if activity exists but not first elem
                     choosePreferences(T,L1), % get preferences of tail
                     append([H],L1,L2),       % add first elem to preferences
                     possibleSubset(L2,ChosenPrefs). % get all possible subsets of it
choosePreferences([activity(H)|T],ChosenPrefs):-
                     possibleSubset(activity(H),L1),% get all possible subsets of activity
                     possibleSubset(T,L2), % get all possible subsets of tail
                     append([L1],L2,L3), % append them together
                     permutation(L3,ChosenPrefs). % permutations of them

%preferenceSatisfaction(Offer,Customer,ChosenPrefs,S)
preferenceSatisfaction(offer(D,L,C,VF,VT,period(P1,P2),DU,NG), customer(FN, LN, BD, ST, NC, J), [H|T], S):-
                     offerMean(offer(D,L,C,VF,VT,period(P1,P2),DU,NG),Mean), % get mean of offer
                     offerAccommodation(offer(D,L,C,VF,VT,period(P1,P2),DU,NG),Accom), %get accom of offer
                     getMean([H|T],M1), %get mean of prefs
                     getSatMean(customer(FN, LN, BD, ST, NC, J),M1,Mean,S1), %get s value of mean
                     getAccom([H|T],Accom1),%get accom of prefs
                     getSatAccom(customer(FN, LN, BD, ST, NC, J),Accom,Accom1,S2), %get s value of accom
                     getAct([H|T],L1), %get list of activity in prefs
                     intersection(L,L1,LF), %see what's common between it and activites in offer
                     getSatAct(customer(FN, LN, BD, ST, NC, J),LF,S3), %get s value of activity
                     S is S1 +S2+S3. %add all
                 
                 
% how much he likes activity based on customer and prefs list
getSatAct(_,[],0).
getSatAct(customer(FN, LN, BD, ST, NC, J),[H|T],S):-
                     getSatAct(customer(FN, LN, BD, ST, NC, J),T,S1),
                     customerPreferredActivity(customer(FN, LN, BD, ST, NC, J),H,S2),
                     S is S1 +S2.



%get list inside activity() of prefs list
getAct([],[]).
getAct([H|T],L1):-
                     H=activity(L1).
getAct([H|T],L1):-
                     H\=activity(_),
                     getAct(T,L1).





% how much he likes accom based on customer,accom in offer and accom in prefs list
getSatAccom(customer(FN, LN, BD, ST, NC, J),Accom,Accom1,S2):-
                     Accom=Accom1,                %when accom of offer and accom of prefs are same
                     customerPreferredAccommodation(customer(FN, LN, BD, ST, NC, J),Accom,S2).
getSatAccom(customer(FN, LN, BD, ST, NC, J),Accom,Accom1,S2):-
                     Accom\=Accom1,
                     S2=0.



%get accom of prefs list if empty then nil
getAccom([],nil).
getAccom([H|T],Accom):-
                     H=accommodation(Accom).
getAccom([H|T],Accom):-
                     H\= accommodation(_),
                     getAccom(T,Accom).



% how much he likes accom based on customer,means of offer and mean in prefs list
getSatMean(customer(FN, LN, BD, ST, NC, J),M1,Mean,S1):-
                     M1=Mean,
                     customerPreferredMean(customer(FN, LN, BD, ST, NC, J),M1,S1).
getSatMean(customer(FN, LN, BD, ST, NC, J),M1,Mean,S1):-
                    M1\=Mean,
                    S1=0.



%get mean of prefs list if empty then nil
getMean([],nil).
getMean([H|T],Mean):-
                     H=means(Mean).
getMean([H|T],Mean):-
                     H\=means(_),
                     getMean(T,Mean).




%Period overlap
overlapPeriod(period(X,Y),period(A,B)):-
                     X@>A,
                     X@<B.
overlapPeriod(period(X,Y),period(A,B)):-
                     Y@>A,
                     Y@<B.
    
    
%get offer
getOffer(L,Offer):-
                     getMean(L,Mean),
                     meanOfOffer(Offer,Mean),
                     getAccom(L,Accom),
                     accomOfOffer(Offer,Accom),
                     getAct(L,Act),
                     activityOfOffer(Offer,Act),
                     getDest(L,Des),
                     desOfOffer(Offer,Des),
                     getBudget(L,B),
                     budgetOfOffer(Offer,B),
                     getPeriod(L,P),
                     periodOfOffer(Offer,P).

%get offer with overlapping period p
periodOfOffer(Offer,P):-
                     P \=nil,
                     offerMean(Offer2,_),
                     Offer2 = offer(_,_,_,_,_,P1,_,_),
                     overlapPeriod(P,P1),
                     Offer=Offer2.
periodOfOffer(Offer,nil):-
                     offerMean(Offer,_).




%get offer with budget B
budgetOfOffer(Offer,B):-
                     B\=nil,
                     offerMean(Offer,_),
                     Offer = offer(_,_,C,_,_,_,_,_),
                     C=<B.
budgetOfOffer(Offer,nil):-
                     offerMean(Offer,_).



%get offer with destination D
desOfOffer(Offer,D):-
                     D\=nil,
                     offerMean(Offer,_),
                     Offer = offer(D,_,_,_,_,_,_,_).
desOfOffer(Offer,nil):-
                     offerMean(Offer,_).



%get offer with activities L
activityOfOffer(Offer,L):-
                     L\=[],
                     offerMean(Offer2,_),
                     Offer2 = offer(_,L1,_,_,_,_,_,_),
                     possibleSubset(L1,L),
                     Offer=Offer2.
activityOfOffer(Offer,[]):-
                     offerMean(Offer,_).



%get offer with accomadation accom
accomOfOffer(Offer,Accom):-
                     Accom\=nil,
                     offerAccommodation(Offer,Accom).
accomOfOffer(Offer,nil):-
                     offerAccommodation(Offer,_).






%get offer with means Mean
meanOfOffer(Offer,Mean):-
                     Mean\=nil,
                     offerMean(Offer,Mean).
meanOfOffer(Offer,nil):-
                     offerMean(Offer,_).



%get dest of prefs list if empty then nil
getDest([],nil).
getDest([H|T],D):-
                     H = dest(D).
getDest([H|T],D):-
                     H \=dest(_),
                     getDest(T,D).


%get budget of prefs list if empty then nil
getBudget([],nil).
getBudget([H|T],B):-
                     H = budget(B).
getBudget([H|T],B):-
                     H\=budget(_),
                     getBudget(T,B).

%get period of prefs list if empty then nil
getPeriod([],nil).
getPeriod([period(X,Y)|T],period(X,Y)).
getPeriod([H|T],L):-
                     H\=period(_,_),
                     getPeriod(T,L).


%recommendOfferForCustomer
recommendOfferForCustomer(Prefs, ChosenPrefs, O):-
                     choosePreferences(Prefs,ChosenPrefs),
                     getOffer(ChosenPrefs,O).


%recommendOffer
recommendOffer([],[],_,[]).
recommendOffer([H|T], [H1|T1], O, C):-
                     recommendOfferForCustomer(H1,Ch,O),
                     O = offer(_,_,_,_,_,_,_,NG),
                     getListOfSat([H|T],O,L,Ch),
                     keysort(L,L2),
                     reverse(L2,L3),
                     getMaxS(L3,L4),
                     getFirstNthElem(L4,NG,C),
                     recommendOffer(T,T1,O2,C2).



getMaxS([],[]).
getMaxS([H-S|T],L):-
                    getMaxS(T,L1),
                    append([S],L1,L).

getListOfSat([],_,[],_).
getListOfSat([H|T],O,L,ChosenPrefs):-
                    preferenceSatisfaction(O, H, ChosenPrefs, S),
                    L1 = [S-H],
                    getListOfSat(T,O,L2,ChosenPrefs),
                    append(L1,L2,L).
getFirstNthElem(_,0,[]).
getFirstNthElem([H|T],N,L):-
                    length([H|T],S),
                    S<N,
                    L = [H|T].
getFirstNthElem([H|T],N,L):-
                    N>0,
                    length([H|T],S),
                    S>=N,
                    N1 is N-1,
                    getFirstNthElem(T,N1,L1),
                    append([H],L1,L).
        
        
        
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getAllActivities(L):-
               setof(X,Y^Z^customerPreferredActivity(Y,X,Z),L).

mostPreferredActivity(C,A):-
                            getAllActivities(L),
                            getAllSat(C,L,L2),
                            sort2(L2,L3), %sort list based on the S number in the [S-Act] structure
                            reverse(L3,L4), % reverse to sort from bigger to smaller
                            getFirstElem(L4,A). % return the first Act from the [S-Act] structure list as it's with highest value
                            
sort2(List,Sorted):-
                    permutation(List,Sorted),is_sorted(Sorted).
is_sorted([]).
is_sorted([S-H]).
is_sorted([S-H,S2-H2|T]):-S=<S2,is_sorted([S2-H2|T]).

getAllSat(_,[],[]).
getAllSat(C,[H|T],R):-
                customerPreferredActivity(C,H,S),
                getAllSat(C,T,R2),
                R = [S-H|R2]. %put satisfaction value next to activity as a structure in form S-Act
        
getFirstElem([S-H|T],H).
        
