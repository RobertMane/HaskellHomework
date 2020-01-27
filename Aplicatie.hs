-- Program care afiseaza frecventa cu care apar cuvintele intr-un fisier text

import System.Console.ANSI
import System.IO
import Data.List(sort)
import Data.Char
import System.Directory

-- Contorizare aparitii cuvant
-- in cazul in care lista de cuvinte nu este ordonata    
conaps::[Char]->[[Char]]->Int
conaps _ []=0
conaps s (c:rl)
    |(s==c)=1+conaps s rl
    |otherwise=conaps s rl

-- Contorizare aparitii cuvant
-- in cazul in care lista de cuvinte este ordonata    
conaps_opt::[Char]->[[Char]]->Int
conaps_opt _ []=0
conaps_opt s (c:rl)
    |(s==c)=1+conaps_opt s rl
    |otherwise=0
	


-- Eliminare aparitii cuvant speciifcat
-- in cazul in care lista este neordonata
delaps::String->[String]->[String]
delaps _ []=[]
delaps s (c:rl)
    |(s==c)=delaps s rl
    |otherwise=c:(delaps s rl)    

-- Eliminare aparitii cuvant specificat
-- in cazul in care lista de cuvinte este sortata
delaps_opt::String->[String]->[String]
delaps_opt _ []=[]
delaps_opt s (c:rl)
    |(s==c)=delaps_opt s rl
    |otherwise=c:rl    

-- Contorizare aparitii cuvinte
conapsg::[String]->[(String,Int)]
conapsg []=[]
conapsg (c:rl)=(c,1+(conaps_opt c rl)):conapsg (delaps_opt c rl)

-- Generare raport in fisier
-- de handle specificat
fGenRap::Handle->[(String,Int)]->IO()
fGenRap h []=do
              hSpace h 30;
              hPutStrLn h "--------------------------";
              hClose h
fGenRap h ((cuv,frec):rl)=do
           hSpace h 30;
	       hPutStr h "| ";
           hputColoana h cuv 8;
	       hPutStr h "|";
	       hSpace h 5;
           hputColoana h (show frec) 9;
           hPutStrLn h "|";
           fGenRap h rl

-- Afisare coloana
-- Varianta fisier           
hputColoana::Handle->String->Int->IO()
hputColoana h cuv ncar=do
                   hPutStr h cuv
                   hFlush h
                   hSpace h (ncar-(length cuv))            

-- Adaugare spatii in scop de aliniere
-- Varianta fisier
hSpace::Handle->Int->IO()
hSpace h 0=hPutStr h ""
hSpace h 1=hPutStr h " "
hSpace h n=hPutStr h " ">>hSpace h (n-1)

-- Afisare statistica
-- Varianta ecran
print_rez::[(String,Int)]->IO()
print_rez []=do 
      space 50;
      putStrLn "--------------------------"
print_rez ((cuv,frec):rl)=do
      space 50;
	  putStr "| ";
      putColoana cuv 8;
	  putStr "|";
	  space 5;
      putColoana (show frec) 9;
      putStrLn "|";
      print_rez rl

-- Afisare coloana
-- Varianta ecran
putColoana::String->Int->IO()
putColoana cuv ncar=do
                   putStr cuv
                   hFlush stdout
                   space (ncar-(length cuv))

-- Adaugare spatii in scop de aliniere
-- Varianta ecran
space::Int->IO()
space 0=putStr ""
space 1=putStr " "
space n=putStr " ">>space (n-1)

-- Afisare text cu centrare
-- in spatiu de lungime data
print_centr::String->Int->IO()
print_centr str npoz
   |(npoz-(length str))>0=do{
                             lcuv<-return (length str);
                             lspace<-return ((npoz-lcuv) `div` 2);
                             space lspace;
                             putStr str;
                             hFlush stdout}
   |otherwise=putStr str                    

-- Convertire cuvinte la lower case
-- pentru uniformizarea sortarii
stolower::[String]->[String]
stolower []=[]
stolower (c:rl)=(map toLower c):(stolower rl)

-- Transformare string in lista de cuvinte
-- fara a lua in calcul semnele de punctuatie
elimnonlit::String->[String]
elimnonlit = words.(filter (\caracter -> isLetter caracter || isSpace caracter))

-- Afisare cuvintelor dintr-o lista
printelemlis::[String]->IO()
printelemlis [] = do return()
printelemlis (c:rl)=do{
                       space 25;
                       putStrLn c;
					   hFlush stdout;
					   printelemlis (delaps c rl)
                      }
-- Verificare daca exista element in lista
existelemlista::String->[String]-> IO Bool
existelemlista _ []=return False
existelemlista cuv (c:rl)
     |(cuv==c)=return True
     |otherwise=existelemlista cuv rl
	 
	 
-- 1) AFISAREA CUVANTULUI/CUVINTELOR CU NUMAR MAXIM DE APARITII DIN FISIER
                  
-- Functie care returneaza lista cu cuvantul/cuvintele care apar 
-- de cele mai multe ori in fisier
cuvcunrmaxdeapar::[(String,Int)]->[(String,Int)]->[String]
cuvcunrmaxdeapar lista []=[]
cuvcunrmaxdeapar lista ((cuv,frec):rl)
     |(frec == nrmaxdeapar (lista))=cuv:cuvcunrmaxdeapar lista (delper (cuv,frec) rl)
     |otherwise=cuvcunrmaxdeapar lista (delper (cuv,frec) rl)
	 
      
-- Functie care calculeaza frecventa maxima
-- dintre toate cuvintele
nrmaxdeapar::[(String,Int)]->Int
nrmaxdeapar [(cuv,frec)]=frec
nrmaxdeapar ((cuv,frec):rl)
     |(nrmaxdeapar rl) > frec = nrmaxdeapar rl
	 |otherwise = frec


-- 2) AFISAREA CUVANTULUI/CUVINTELOR CU NUMAR MINIM DE APARITII DIN FISIER

-- Functie care returneaza lista cu cuvantul/cuvintele care apar 
-- de cele mai putine ori in fisier
cuvcunrmindeapar::[(String,Int)]->[(String,Int)]->[String]
cuvcunrmindeapar lista []=[]
cuvcunrmindeapar lista ((cuv,frec):rl)
     |(frec == nrmindeapar (lista))=cuv:cuvcunrmindeapar lista (delper (cuv,frec) rl)
     |otherwise=cuvcunrmindeapar lista (delper (cuv,frec) rl)
	 
      
-- Functie care calculeaza frecventa minima
-- dintre toate cuvintele
nrmindeapar::[(String,Int)]->Int
nrmindeapar [(cuv,frec)]=frec
nrmindeapar ((cuv,frec):rl)
     |(nrmindeapar rl) < frec = nrmindeapar rl
	 |otherwise = frec



-- Eliminare aparitii cuvant specificat 
-- din lista
delper::(String,Int)->[(String,Int)]->[(String,Int)]
delper _ []=[]
delper s (c:rl)
    |(s==c)=delper s rl
    |otherwise=c:(delper s rl)  
	
	
-- Functie care afiseaza formatat continutul
-- unei liste de string-uri
afisareform::[String]->IO()
afisareform [] = do return()
afisareform (c:rl)=do{
                     space 40;
		             putStr"|";
		             space 10;
		             putColoana c 15;
		             putStrLn"|";
					 afisareform rl
                    }

-- FUNCTIA CARE SE EXECUTA LA PORNIREA PROGRAMULUI
		  
ruleazaProgram::String->IO()
ruleazaProgram numeFisier=do{
                             fileExists<-doesFileExist numeFisier;
							 if fileExists == True
							   then do{
		                               meniuOptiuni numeFisier
		                              }
							   else do{
							           clearScreen;
							           setCursorPosition 12 40; 
							           putStrLn "NU EXISTA UN FISIER CU NUMELE SPECIFICAT!";
									   hFlush stdout;
		     	                       temp<-getLine;
		                               putStrLn temp;
									   hFlush stdout;
									   main
									  }
                            } 
    
afisaparcuvant::String->[String]->IO()
afisaparcuvant cuv listCuv=do{
                      
				   
	                  putStrLn "";
                	  hFlush stdout;			  
	                  putStrLn "";
	                  hFlush stdout;
					  
					  space 25;
					  putStrLn "-------------------------------";
					  hFlush stdout;
					  space 25;
                      putStrLn "| CUVANT ALES   |   FRECVENTA |";
					  hFlush stdout;
					  space 25;
					  putStrLn "-------------------------------";
					  hFlush stdout;
					  
					   
					  space 25;
					  putStr "| ";
					  hFlush stdout;
					  
					  putColoana cuv 14;
					  
					  putStr "|";
					  hFlush stdout;
					  
					  space 3;
					  putStr (show(conaps cuv listCuv));
					  
					  space 9;
					  putStr "|";
					  hFlush stdout;
					  
					  putStrLn"";
					  hFlush stdout;
					  
					  space 25;
					  putStrLn "-------------------------------";
					  hFlush stdout;
					  
					                      
		              temp<-getLine;
		              putStrLn temp
					  
                     }      
-- Optiune pentru a afisa fiecare cuvant impreuna cu frecventa acestuia in format tabelar
opt1::String->IO()
opt1 numeFisier=do{
                   clearScreen;

                   -- Transformare continut fisier de test 
                   -- in sir de caractere
                   sirCar<-readFile numeFisier;

                   -- Transformare sir de caractere 
                   -- in lista de cuvinte
		           lisCuv<-return (elimnonlit sirCar);
		 
                   -- Transformare cuvinte din lista lisCuv
                   -- in varianta lor lower case
                   llower<-return (stolower lisCuv);

                   -- Sortare lista de cuvinte llower
                   -- pentru a optimiza generarea statisticii         
                   lCuvS<-return (sort llower);

                   putStrLn "";
                   putStrLn "";
       
                   -- Determinarea statisticii       
                   rez<-return (conapsg lCuvS);
       
                   -- Afisare statistica pe ecran
				   setCursorPosition 4 50;
                   putStrLn "FRECVENTA CUVINTELOR";
                   hFlush stdout;
				   setCursorPosition 5 50;
                   putStrLn ("IN FISIERUL:"++numeFisier);
                   hFlush stdout;
                   putStrLn "";
                   putStrLn "";

                   setCursorPosition 7 50;
                   putStrLn "--------------------------";
				   setCursorPosition 8 50;
                   putStrLn "| CUVANT   |   FRECVENTA |";
				   setCursorPosition 9 50;
                   putStrLn "--------------------------";
                   print_rez rez;
		
		           temp<-getLine;
		           putStrLn temp
                  }
				  
-- Optiune pentru afisarea raportului de statistica in
-- in fisierul <Raport.dat>
opt2::String->IO()
opt2 numeFisier=do{
                   clearScreen;
				   
				   -- Transformare continut fisier de test 
                   -- in sir de caractere
                   sirCar<-readFile numeFisier;

                   -- Transformare sir de caractere 
                   -- in lista de cuvinte
		           lisCuv<-return (elimnonlit sirCar);
		 
                   -- Transformare cuvinte din lista lisCuv
                   -- in varianta lor lower case
                   llower<-return (stolower lisCuv);

                   -- Sortare lista de cuvinte llower
                   -- pentru a optimiza generarea statisticii         
                   lCuvS<-return (sort llower);

                   putStrLn "";
                   putStrLn "";
       
                   -- Determinarea statisticii       
                   rez<-return (conapsg lCuvS);
				   
				   -- Deschiderea fisierului pentru scriere
				   
                   h<-openFile "Raport.dat" WriteMode;
				   
				   hSpace h 30;
                   hPutStrLn h "--------------------------";
				   hSpace h 30;
                   hPutStrLn h "| CUVANT   |   FRECVENTA |";
				   hSpace h 30;
                   hPutStrLn h "--------------------------";
                   fGenRap h rez;
				   
                   setCursorPosition 12 40; 
                   putStr "RAPORT DE STATISTICA CREAT CU SUCCES!";
                   hFlush stdout;
		           temp<-getLine;
		           putStrLn temp
                  }



-- Optiune pentru a afisa frecventa unui singur cuvant
opt3::String->IO()
opt3 numeFisier=do{
                   clearScreen;
		           -- Transformare continut fisier de test 
                   -- in sir de caractere
                   sirCar<-readFile numeFisier;

                   -- Transformare sir de caractere  
                   -- in lista de cuvinte
		           lisCuv<-return (elimnonlit sirCar);
		 
                   -- Transformare cuvinte din lista lisCuv
                   -- in varianta lor lower case
                   llower<-return (stolower lisCuv);

                   -- Sortare lista de cuvinte llower
                   -- pentru a optimiza generarea statisticii         
                   lCuvS<-return (sort llower);
		
		
		           setCursorPosition 2 40;
		           putStrLn "AFISARE FRECVENTA PENTRU UN SINGUR CUVANT";
		           hFlush stdout;
		
		           setCursorPosition 6 25;
		           putStrLn "Cuvintele din text sunt urmatoarele:";
		           hFlush stdout;
                   printelemlis lCuvS;

                   putStrLn "";
                   space 25;				   
		           putStr "Va rugam sa introduceti cuvantul pentru care doriti sa aflati frecventa: ";
		           hFlush stdout;
		           cuv<-getLine;
				   excuvinlista<-existelemlista cuv lCuvS;
				   
				   if excuvinlista == False 
				      then do{clearScreen;
					       setCursorPosition 12 30;
					       putStrLn "CUVANTUL INTRODUS NU EXISTA IN LISTA ! VA RUGAM SA REINCERCATI !";
					       hFlush stdout;
						   temp<-getLine;
		                   putStrLn temp;
						   hFlush stdout;
						   opt2 numeFisier;
						   }
				       else afisaparcuvant cuv lCuvS;
					       
				       
                  }

-- Optiune pentru a afisa cuvantul cu cele mai multe aparitii si de cate ori apare acesta
opt4::String->IO()
opt4 numeFisier=do{
        clearScreen;
		-- Transformare continut fisier de test 
        -- in sir de caractere
        sirCar<-readFile numeFisier;

        -- Transformare sir de caractere  
        -- in lista de cuvinte
		lisCuv<-return (elimnonlit sirCar);
		 
        -- Transformare cuvinte din lista lisCuv
        -- in varianta lor lower case
        llower<-return (stolower lisCuv);

        -- Sortare lista de cuvinte llower
        -- pentru a optimiza generarea statisticii         
        lCuvS<-return (sort llower);
		
		-- Lista cuvintelor cu cele mai multe aparitii din text
		lisMax<-return (cuvcunrmaxdeapar (conapsg lCuvS) (conapsg lCuvS));
		
        setCursorPosition 2 40;  
		
        putStrLn "CUVANTUL/CUVINTELE CU CELE";
		hFlush stdout;
		space 40;
		putStrLn "MAI MULTE APARITII DIN TEXT";
		hFlush stdout;
		
		putStrLn"";
		hFlush stdout;
		putStrLn"";
		hFlush stdout;
		putStrLn"";
		hFlush stdout;
		
		space 40;
		putStrLn"---------------------------";
		
		
		space 40;
		putStrLn"|     CUVANT/CUVINTE      |";
		
		space 40;
		putStrLn"---------------------------";
		
		afisareform lisMax;
		
		space 40;
		putStrLn"---------------------------";
		
		putStrLn"";
		putStrLn"";
		putStrLn"";
		
		space 40;
		putStrLn"---------------------------";
		
		space 40;
		putStrLn"|     NUMAR APARITII      |";
		
		space 40;
		putStrLn"---------------------------";
		
		space 40;
		putStr"|";
		space 12;
		putColoana (show (nrmaxdeapar(conapsg lCuvS))) 13;
		putStrLn"|";
		
		space 40;
		putStrLn"---------------------------";
        
		temp<-getLine;
		putStrLn temp
       }

-- Optiune pentru a afisa cuvantul cu cele mai putine aparitii si de cate ori apare acesta
opt5::String->IO()
opt5 numeFisier=do{
         clearScreen;
		-- Transformare continut fisier de test 
        -- in sir de caractere
        sirCar<-readFile numeFisier;

        -- Transformare sir de caractere  
        -- in lista de cuvinte
		lisCuv<-return (elimnonlit sirCar);
		 
        -- Transformare cuvinte din lista lisCuv
        -- in varianta lor lower case
        llower<-return (stolower lisCuv);

        -- Sortare lista de cuvinte llower
        -- pentru a optimiza generarea statisticii         
        lCuvS<-return (sort llower);
		
		-- Lista cuvintelor cu cele mai putine aparitii din text
		lisMin<-return (cuvcunrmindeapar (conapsg lCuvS) (conapsg lCuvS));
        
		
		setCursorPosition 2 40;  
		
        putStrLn "CUVANTUL/CUVINTELE CU CELE";
		hFlush stdout;
		space 40;
		putStrLn "MAI PUTINE APARITII DIN TEXT";
		hFlush stdout;
		
		putStrLn"";
		hFlush stdout;
		putStrLn"";
		hFlush stdout;
		putStrLn"";
		hFlush stdout;
		
		space 40;
		putStrLn"---------------------------";
		
		
		space 40;
		putStrLn"|     CUVANT/CUVINTE      |";
		
		space 40;
		putStrLn"---------------------------";
		
		afisareform lisMin;
		
		space 40;
		putStrLn"---------------------------";
		
		putStrLn"";
		putStrLn"";
		putStrLn"";
		
		space 40;
		putStrLn"---------------------------";
		
		space 40;
		putStrLn"|     NUMAR APARITII      |";
		
		space 40;
		putStrLn"---------------------------";
		
		space 40;
		putStr"|";
		space 12;
		putColoana (show (nrmindeapar(conapsg lCuvS))) 13;
		putStrLn"|";
		
		space 40;
		putStrLn"---------------------------";
        
		temp<-getLine;
		putStrLn temp
       }
-- Optiune pentru a iesi din program    
opt7::IO()
opt7=do{
        clearScreen;
        setCursorPosition 12 50; 
        putStr "LA REVEDERE !";
        hFlush stdout;
		temp<-getLine;
		putStrLn temp
       }

optInvalida=do{
               clearScreen;
               setCursorPosition 1 0; 
               putStr "Optiunea aleasa este invalida ! Va rugam sa reincercati !";
               hFlush stdout;
		       temp<-getLine;
		       putStrLn temp
			  }
	 
execopt::String->String->IO()	 
execopt opt numeFisier | (opt=="1")=do{
                                       opt1 numeFisier;
							           meniuOptiuni numeFisier
                                      }
									  
execopt opt numeFisier | (opt=="2")=do{
                                       opt2 numeFisier;
                                       meniuOptiuni numeFisier							 
                                      }
				

execopt opt numeFisier | (opt=="3")=do{
                                       opt3 numeFisier;
                                       meniuOptiuni numeFisier							 
                                      }
				
execopt opt numeFisier | (opt=="4")=do{
                                       opt4 numeFisier;
							           meniuOptiuni numeFisier
							          }

execopt opt numeFisier | (opt=="5")=do{
                                       opt5 numeFisier;
							           meniuOptiuni numeFisier
							          }
									 
execopt opt numeFisier | (opt=="6")=do{
                                       main;
							          }									 
execopt opt numeFisier | (opt=="7")=do{
                                       opt7;
							          }
							 
execopt opt numeFisier=do{
                          optInvalida;
						  meniuOptiuni numeFisier
						 }

meniuOptiuni::String->IO() 
meniuOptiuni numeFisier=do{
		clearScreen;
		setCursorPosition 2 50;
		putStrLn "MENIU PRINCIPAL";
		hFlush stdout;
		setCursorPosition 5 20;
		putStrLn "Va rugam sa selectati una dintre optiunile de mai jos :";
		hFlush stdout;
		setCursorPosition 7 20;
		putStrLn "1. Afisarea tabelara a tuturor cuvintelor din fisier impreuna cu frecventele lor.";
		hFlush stdout;
		setCursorPosition 8 20;
		putStrLn "2. Afisarea raportului de statistica a cuvintelor in fisier.";
		hFlush stdout;
		setCursorPosition 9 20;
		putStrLn "3. Afisarea unui anumit cuvant impreuna cu numarul sau de aparitii din text.";
		hFlush stdout;
		setCursorPosition 10 20;
		putStrLn "4. Afisarea cuvantului cu cele mai multe aparitii din text si de cate ori apare acesta.";
		hFlush stdout;
		setCursorPosition 11 20;
		putStrLn "5. Afisarea cuvantului cu cele mai putine aparitii din text si de cate ori apare acesta.";
		hFlush stdout;
		setCursorPosition 12 20;
		putStrLn "6. Pentru a va intoarce la meniul de alegere a fisierului.";
		hFlush stdout;
		setCursorPosition 13 20;
		putStrLn "7. Pentru a iesi din program.";
		hFlush stdout;
		setCursorPosition 16 20;
		putStr "Optiunea dvs.: ";
		hFlush stdout;
		opt<-getLine;
		execopt opt numeFisier
        }		

main::IO()		
main=do{
        setTitle "Afisare Frecvente Cuvinte";
		clearScreen;
		setCursorPosition 2 40;
	    putStr "ALEGEREA FISIERULUI DE LUCRU";
        hFlush stdout;
		setCursorPosition 8 20;
	    putStr "Va rugam sa introduceti numele fisierului pe care doriti sa il analizati: ";
        hFlush stdout;
        numeFisier<-getLine;
		ruleazaProgram numeFisier			
	   }		  
