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
              hPutStrLn h "*************************"
              hClose h
fGenRap h ((cuv,frec):rl)=do
           hputColoana h cuv 15
           hputColoana h (show frec) 9
           hPutStrLn h "|"
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
print_rez []=putStrLn "**************************"
print_rez ((cuv,frec):rl)=do
      putColoana cuv 15
      putColoana (show frec) 9
      putStrLn "|"
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
	 
-- 3) AFISAREA CUVANTULUI/CUVINTELOR CU NUMAR MAXIM DE APARITII DIN FISIER
                  
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


-- 4) AFISAREA CUVANTULUI/CUVINTELOR CU NUMAR MINIM DE APARITII DIN FISIER

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
	

-- FUNCTIA CARE SE EXECUTA LA PORNIREA PROGRAMULUI
		  
ruleazaProgram::String->IO()
ruleazaProgram numeFisier=do{
                             fileExists<-doesFileExist numeFisier;
							 if fileExists == True
							   then do{
		                               meniuOptiuni numeFisier
		                              }
							   else do{
							           putStrLn "Nu exista un fisier cu numele specificat";
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
					  
                      putStr "Cuvantul ales ,impreuna cu numarul sau de aparitii, este:";
					  hFlush stdout;
					  
					  putStrLn "";	
					  hFlush stdout;
					  
	                  putStrLn "";
					  hFlush stdout;
					  
					  putStr cuv;
					  hFlush stdout;
					  
					  putStr " ";
					  hFlush stdout;
					  putStr " ";
					  hFlush stdout;
					  
					  print (conaps cuv listCuv);
					                      
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
                   print_centr (map toUpper "Frecventa cuvintelor") 26;
                   putStrLn "";
                   print_centr ("in fisierul:"++numeFisier) 26;

                   putStrLn "";
                   putStrLn "";

                   putStrLn "**************************";
                   putStrLn "Cuvant         Frecventa ";
                   putStrLn "**************************";
                   print_rez rez;
       
                   -- Generare raport statistic cu salvare 
                   -- in fisierul <Raport.dat>
                   h<-openFile "Raport.dat" WriteMode;
                   hPutStrLn h "**************************";
                   hPutStrLn h "Cuvant         Frecventa ";
                   hPutStrLn h "**************************";
                   fGenRap h rez;
		
                   setCursorPosition 1 0; 
                   putStr "Cuvintele din text, impreuna cu frecventele acestora, sunt urmatoarele:";
                   hFlush stdout;
		           temp<-getLine;
		           putStrLn temp
                  }

-- Optiune pentru a afisa frecventa unui singur cuvant
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
		
		           setCursorPosition 1 0;
		           putStrLn "Cuvintele din text sunt urmatoarele:";
		           hFlush stdout;
                   printelemlis lCuvS;

                   putStrLn "";		
		           putStr "Va rugam sa introduceti cuvantul pentru care doriti sa aflati frecventa:";
		           hFlush stdout;
		           cuv<-getLine;
				   excuvinlista<-existelemlista cuv lCuvS;
				   
				   if excuvinlista == False 
				      then do{putStrLn "Cuvantul introdus nu exista in lista ! Va rugam sa reincercati !";
					       hFlush stdout;
						   temp<-getLine;
		                   putStrLn temp;
						   hFlush stdout;
						   opt2 numeFisier;
						   }
				       else afisaparcuvant cuv lCuvS;
					       
				       
                  }

-- Optiune pentru a afisa cuvantul cu cele mai multe aparitii si de cate ori apare acesta
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
		
		-- Lista cuvintelor cu cele mai multe aparitii din text
		lisMax<-return (cuvcunrmaxdeapar (conapsg lCuvS) (conapsg lCuvS));
        setCursorPosition 1 0;  
		
        putStr "Cuvantul/cuvintele cu cele mai multe aparitii din text:";
		printelemlis lisMax;
        hFlush stdout;
		temp<-getLine;
		putStrLn temp
       }

-- Optiune pentru a afisa cuvantul cu cele mai putine aparitii si de cate ori apare acesta
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
		
		-- Lista cuvintelor cu cele mai putine aparitii din text
		lisMin<-return (cuvcunrmindeapar (conapsg lCuvS) (conapsg lCuvS));
        setCursorPosition 1 0;  
		
        putStr "Cuvantul/cuvintele cu cele mai putine aparitii din text:";
		printelemlis lisMin;
        hFlush stdout;
		temp<-getLine;
		putStrLn temp
       }
-- Optiune pentru a iesi din program    
opt5::IO()
opt5=do{
        clearScreen;
        setCursorPosition 1 0; 
        putStr "La revedere !";
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
                                       opt5;
							          }
							 
execopt opt numeFisier=do{
                          optInvalida;
						  meniuOptiuni numeFisier
						 }

meniuOptiuni::String->IO() 
meniuOptiuni numeFisier=do{
		clearScreen;
		hFlush stdout;
		setCursorPosition 1 0;
		putStrLn "Va rugam sa selectati una dintre optiunile de mai jos :";
		hFlush stdout;
		setCursorPosition 3 0;
		putStrLn "1. Afisarea tabelara a tuturor cuvintelor din fisier impreuna cu frecventele lor.";
		hFlush stdout;
		putStrLn "2. Afisarea unui anumit cuvant impreuna cu numarul sau de aparitii din text.";
		hFlush stdout;
		putStrLn "3. Afisarea cuvantului cu cele mai multe aparitii in text si de cate ori apare acesta.";
		hFlush stdout;
		putStrLn "4. Afisarea cuvantului cu cele mai putine aparitii in text si de cate ori apare acesta.";
		hFlush stdout;
		putStrLn "5. Pentru a iesi din program.";
		hFlush stdout;
		opt<-getLine;
		execopt opt numeFisier
        }		

main::IO()		
main=do{
        setTitle " Afisare Frecvente Cuvinte";
		clearScreen;
		setCursorPosition 1 0;
	    putStr "Va rugam sa introduceti numele fisierului pe care doriti sa il analizati:";
        hFlush stdout;
        numeFisier<-getLine;
		ruleazaProgram numeFisier			
	   }		  
