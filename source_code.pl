% Define the movie/12 predicate
:- dynamic(movie/12).

%start the predicate main immediately when consulting 
:- initialization main.

main :-
      write('Loading data from the csv file...'), nl,
      load,
      write('Data is Loaded! type start to initialize the recommendation system.'), nl.
% loading the csv file into prolog facts using the "load" predicate
% the fact that forms the knowledge base is movie/12
% movie(Id,Type,Title,Director,Cast,Country,Date_added,Release_year,Rating,Duration,Category,Description).
load:-
      csv_read_file('database.csv', Rows, [functor(movie), arity(12)]), maplist(assert, Rows).

% a list that contains categories of movies
category(['Documentaries', 'Children' , 'Comedies', 'Dramas', 'Thrillers', 'Romantic', 'Music', 'Horror' ,'Sci-Fi' ,'Fantasy' ,'Action' , 'Classic' ,'Adventure' ,'Anime' ,'Features', 'Sports','Comedy']).

% a list that contains categories of Tv Shows.
category_show(['International TV Shows', 'Mysteries', 'Crime', 'Action & Adventure', 'Docuseries', 'Reality', 'Romantic', 'Comedies', 'Horror', 'Dramas', 'Adventure', 'Thrillers', 'Sci-Fi', 'Fantasy', 'Anime' , 'Science' ,'Nature' ,'Teen', 'Classic',  'Horror', 'Fantasy']).

% a list that contains ratings
rating(['TV-MA','PG','TV-14','PG-13','TV-PG','TV-Y','TV-Y7','R','TV-G','G','NC-17','NR','TV-Y7-FV','UR']).

%login_admin predicate to store admin credentials login_admin(Username, Password)
login_admin('admin','admin').

% the start rule : the user types start. to initiate the program
start:-
      write('*** Movies and TV shows Recommendation System ***'),nl,
      menu.

% Main menu rule
menu:-
      nl,nl,
      write('Would you like some movie or TV show recommendations? Select a number'),nl,
      write('1- Yes'),nl,
      write('2- No'),nl,
      write('3- Login as an Admin'),nl,
      write('Type in your choice:'),nl,
      read(X),
      option(X).


%if user chooses no
option(2):-
      write('Thank you for using our system!').

%login as an admin
option(3) :-
      write('*Login as an Administrator*'), nl,
      write('Enter Username: '), read(Username),      
      write('Enter Password: '), read(Password), nl,
      (
          login_admin(Username, Password) -> (
            write('Logged in as an Admin successfully!! : '),nl,nl,admin_menu) ; % If login succeeds, go to admin menu
            write('Login Failed, try again..'), nl,                             % Otherwise, print an error message and try again
          option(3)
      ).

option(1):-
      % Showing the messages in the ui and taking input from the user
      write('Select 1 for Movies or 2 for TV shows'),nl,
      write('Type in your choice:'),nl,
      read(A),
      write('To pick a category, Select a number:'),nl,
      %if user chooses movies A=1 , display categories of movies
      (A =:= 1 ->
         category(L),
         (display_list(L,0);true);
      %if user chooses TV Shows A=2 , display categories of tv shows
         category_show(L),
         (display_list(L,0);true)),
         write('Type in your choice:'),nl,
      %read the category input from the user
         read(B),
         write('Enter the preferred year range'),nl,
         write('1- < 1980'),nl,
         write('2- 1980-1989'),nl,
         write('3- 1990-1999'),nl,
         write('4- 2000-2009'),nl,
         write('5- 2010 >'),nl,
         write('Type in your choice:'),nl,
      %read the release year preffered from the user
         read(C),
         write('Pick the rating , Select a number'),nl,
      %read the preffered rating from the user
         rating(R),
         (display_list(R,0);true),
         write('Enter your choice:'),nl,
         read(D),
      % display the length / number of seasons prompt to the user depending on the selected type (Movie/TV Show)
	(A =:= 1 ->  writeln('Select 1 for less than 90 min or 2 for 90 mins or more');
         writeln('Select the preferred number of seasons')),
         write('Enter your choice:'),nl,
      % read the length / number of seasons
         read(E),
         (A =:= 1 ->  nl,writeln('*Movie we recommend for you:'),nl;
         nl,writeln('*TV Show we recommend for you:'),nl),
      % Now, we've collected all the needed input from user, we generate recommendations using the rule recommend
      recommend([A,B,C,D,E], Movies), 
      print_results(Movies,Rest), 
      nl,nl,
      write('Did you like it? type yes or no '),nl,
      read(Response),
      ( Response=no -> start ; 
      nl,write('Here is another recommendation for you:'),nl, 
      print_results(Rest,_),!).

print_results([],_):- nl,write('No recommendation found.'),nl,nl,start.
print_results([H|T],T):-
      movie(H, 'Movie', Title, _, _, Country, _, Release_year, _, Duration, _, Description),
      write('Title: '), write(Title), nl,
      write('Country: '), write(Country), nl,
      write('Release Year: '), write(Release_year), nl,
      write('Duration: '), write(Duration),write(' mins'), nl,
      write('Description: '), write(Description), nl.
print_results([H|T],T):-
      movie(H, 'Show', Title, _, _, Country, _, Release_year, _, Duration, _, Description),
      write('Title: '), write(Title), nl,
      write('Country: '), write(Country), nl,
      write('Release Year: '), write(Release_year), nl,
      write('Duration: '), write(Duration),write(' seasons'),  nl,
      write('Description: '), write(Description), nl.      

% Defining the rule recommend to match the corresponding movies to user preferences
% movie(Id,Type,Title,Director,Cast,Country,Date_added,Release_year,Rating,Duration,Category,Description).
/*    getting input from user
      A=1 is movies A=2 is TV shows
      B is category number
      C is release year range (number)
      D is the rating (number)
      E=1 less than 90min , E=2 more or equal to 90min, or E can be the number of seasons       */

% for Movies less than 90mins
recommend([1, B, C, D, 1],Movies) :-
      findall(ID,(
            year_range(C,Year), % from the year range input (1-2-3-4-5) we generate all the possible year using the predicate year_range
            movie(ID, 'Movie', _, _, _, _, _, Year, Rating, Dur, Categ, _), % matching the corresponding movies 
            P is B-1, nth0(P,Z,ELEM), sub_atom(Categ, _, _, _, ELEM), category(Z),  % matching category, (user input is just numbers from the list, we have to transform them into a string )
            PP is D-1,  nth0(PP,J,ELEMM), atom_concat(ELEMM,_,Rating), rating(J), % matching rating
            Dur < 90 % less than 90min        
      ),Movies).
      
% for Movies greater or equal to 90mins
recommend([1, B, C, D, 2],Movies) :-
      findall(ID,(
            year_range(C,Year), 
            movie(ID, 'Movie', _, _, _, _, _, Year, Rating, Dur, Categ, _),
            P is B-1, nth0(P,Z,ELEM), sub_atom(Categ, _, _, _, ELEM), category(Z),  % matching category
            PP is D-1,  nth0(PP,J,ELEMM), atom_concat(ELEMM,_,Rating), rating(J),        % matching rating
            Dur >= 90
       ),Movies).

% for TV shows
recommend([2,B,C,D,E],Movies):-
      findall(ID,(
            year_range(C,Year),
            movie(ID,'Show',_,_,_,_,_,Year,Rating,E,Categ,_), 
            P is B-1, nth0(P,Z,ELEM), sub_atom(Categ, _, _, _, ELEM), category_show(Z),  % matching category
            PP is D-1,  nth0(PP,J,ELEMM), atom_concat(ELEMM,_,Rating), rating(J)        % matching rating
      ),Movies).

%year range helper predicate
year_range(1,Year):- between(1925,1979,Year).
year_range(2,Year):- between(1980,1989,Year).
year_range(3,Year):- between(1990,1999,Year).
year_range(4,Year):- between(2000,2009,Year).
year_range(5,Year):- between(2010,2023,Year).

% display helper for lists
display_list([],0).
display_list([H|T],A):-
                  write(' '),
                  M is A+1,
                  write(M),
                  write('- '),
                  write(H),nl,
                  display_list(T,M).

%defining the admin menu
admin_menu:-
      write('Select an option :'),nl,
      write('1- Display all Movies/TV shows'),nl,
      write('2- Add a Movies/TV show'),nl,
      write('3- Delete a Movie/TV show'),nl,
      write('4- Logout'),nl,
      read(X),
      admin_option(X).

%to display id and titles for the admin
display_movies_tv_shows :-
    findall(movie(Id, Title), movie(Id, _, Title, _, _, _, _, _, _, _, _, _), Movies),
    display_movies_tv_shows_helper(Movies, 1).
%stop condition: if all movies are displayed, go back to the admin menu.
display_movies_tv_shows_helper([], _) :- admin_menu.
%if we reach 50 or 100 or 150 etc... which is N mod 50, prompt admin to display more or exit
display_movies_tv_shows_helper([movie(Id, Title)|T], N) :-
    N>50,N mod 50 =:= 1,
    write('ID: '), write(Id), write(' - '),
    write('Title: '), write(Title), nl,
    N1 is N+1,
    write('Type anything to view more movies/TV shows, or type exit to return'),
    nl,
    read(Input),
    (Input = 'exit' -> admin_menu 
    ;  display_movies_tv_shows_helper(T, N1) ).
% display next movie if not N mod 50
display_movies_tv_shows_helper([movie(Id, Title)|T], N) :-
    write('ID: '), write(Id), write(' - '),
    write('Title: '), write(Title), nl,
    N1 is N+1,
    display_movies_tv_shows_helper(T, N1).


admin_option(1):- display_movies_tv_shows. 
admin_option(4):- start. % to logout
admin_option(2):- add.
admin_option(3):- delete.

% add a movie/show , taking input from the admin 
add:-
    write('Type 1 to add a Movie or 2 to add a TV Show'),nl,read(N),
    ( N=1,Type='Movie' ; N=2,Type='Show' ),
    write('Enter Id: '), read(Id),
    write('Enter Title: '), read(Title),
    write('Enter Director: '), read(Director),
    write('Enter Cast: '), read(Cast),
    write('Enter Country: '), read(Country),
    write('Enter Date_added: '), read(Date_added),
    write('Enter Release year: '), read(Release_year),
    write('Enter Rating: '), read(Rating),
    write('Enter Duration: '), read(Duration),
    write('Enter Category: '), read(Category),
    write('Enter Description: '), read(Description),
    Movie = movie(Id,Type,Title,Director,Cast,Country,Date_added,Release_year,Rating,Duration,Category,Description),
    % use assert to add the movie/show to prolog memory
    assert(Movie),
    % we user csv_write_stream to write the new row into the csv file
    open('database.csv', append, Stream),
    csv_write_stream(Stream, [Movie], []),
    close(Stream),nl,
    write(Type),write(' added successfully!'),nl,nl,
    admin_menu.

% delelte a movie/show by the id 
% the process here is to read the csv file and put in a list "Rows",
% we remove the movie that has the id to be deleted from that list, also remove it from the prolog memory using retract
% then we write back the new list into the csv file
delete:-
      write('*Delete a Movie/TV show: '),nl,
      write('Enter Id: '), read(Id),
      csv_read_file('database.csv', Rows),
      (
            (    member(row(Id, _, _, _, _, _, _, _, _, _, _, _),Rows), % list with all movies "Rows"
            delete(Rows,row(Id, _, _, _, _, _, _, _, _, _, _, _), FilteredRows), % list after deleting the movie "FilteredRows"
            csv_write_file('database.csv', FilteredRows),  % writing the new list into the csv file
            retract(movie(Id, _, _, _, _, _, _, _, _, _, _, _) ), % removing it from prolog facts
            nl,write('Deleted successfully!'),nl,admin_menu
            ) ;
            ( 
            \+member(row(Id, _, _, _, _, _, _, _, _, _, _, _),Rows), % if the id dosent exist print an error message and go back 
            write('invalid Id, try again..'),nl,admin_menu ) 
      ).
       

      

