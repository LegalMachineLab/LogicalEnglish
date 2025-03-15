:-module('4_affiliates_3',[]).

en("the target language is: prolog.

the predicates are:
    *a term* must not be a variable,
    *a term* must be nonvar,
    *an entity* has affiliated with *an affiliate* at *a date*,
    *a date* is not earlier than *a second date*,
    *an affiliate* is an individual or is a company,
    *an affiliate* is a trust,
    *an affiliate* is a partnership,
    *an affiliate* is a superannuation fund,
    *an affiliate* acts in accordance with directions from *an entity*,
    *an affiliate* acts in concert with *an entity*,
    *an affiliate* is affiliated per older legislation with *an entity*,
    *an entity* is a trust according to other legislation,
    *an entity* is a partnership,
    *an entity* is a partnership according to other legislation,
    *an entity* is a superannuation fund according to other legislation.
    
the ontology is:

an entity is a trust
    if the entity is a trust according to other legislation.

an entity is a partnership
    if the entity is a partnership according to other legislation.

an entity is a superannuation fund
    if the entity is a superannuation fund according to other legislation.

the knowledge base 4_affiliates_3 includes:

an entity has affiliated with an affiliate at a date
    if the date is not earlier than 2009-01-01
    and the affiliate is an individual 
        or the affiliate is a company
    and it is not the case that
        the affiliate is a trust
    and it is not the case that
        the affiliate is a partnership
    and it is not the case that
        the affiliate is a superannuation fund
    and the affiliate acts in accordance with directions from the entity
        or  the affiliate acts in concert with the entity.

an entity has affiliated with an affiliate at a date
    if  the date is known
    and the date is before 2009-01-01
    and the affiliate is affiliated per older legislation with the entity. 
    
a date P is not earlier than a date A
    if P is A
    or 	P is known
    	and A is known
    	and A is before P. 
    
scenario test is:
    company is a company. 
    company acts in accordance with directions from andrew.
    company is affiliated per older legislation with andrew.
    
query one is:
    andrew has affiliated with company at 2020-01-02.

query two is:
    who affiliated with which entity at which date. 

query three is:
    what entity affiliated with which company at which date. 

query four is:
    when did which entity affiliate with which other entity.

query five is:
    on what date did which entity affiliate with which company. 

query six is:
    is there an affiliation between which entity and which company. 

query seven is:
    when did the affiliation between which entity and which company begin.
    
").

/** <examples>
?- show prolog. 
?- answer("one with scenario test"). 
?- answer("two with scenario test"). 
?- answer("three with scenario test"). 
?- answer("four with scenario test"). 
?- answer("five with scenario test"). 
?- answer("six with scenario test"). 
?- answer("seven with scenario test"). 
?- answer(one, with(test), le(E), R).
*/
