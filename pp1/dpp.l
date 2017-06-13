/*
 * file:  dpp.l
 * ------------
 * Lex inupt file to generate the scanner for the preprocessor.
 * If you decide to use lex for the preprocesor, put your rules
 * here, otherwise the file can remain empty.
 */
%{	
	#include <map>
	#include <string>
	#include <algorithm>
	#include "errors.h"
    using namespace std;
    int lineas=1;

    std::map<string, string> diccionario;
%}
comentario "//".*\n
comentarioMultiple   "/*"([^*]*"/*"?[^*]*|[^*]*"*"+[^/])"*/"
errorComentarioMul   "/*"([^*]*"/*"*[^*]*|[^*]*"*"+[^/])
directiva      "#define "[A-Z]+" ".+
expdirectiva  "#"[A-Z]+
errorDirectiva  "#define"[^\n]* 
%%

{comentario}   {;}
{comentarioMultiple}  {string comentarios = strdup(yytext);
					   int num_lineas = count(comentarios.begin(), comentarios.end(), '\n');
					   for(int i = 0; i < num_lineas; i++){
					            printf("%s", "\n");	 
					       }
					   }

{errorComentarioMul}  {ReportError::UntermComment();}

{directiva}			  {string enunciado = strdup(yytext);
					    string inicio = enunciado.substr(8);
					    int delimitador = inicio.find(" ");
						string token = inicio.substr(0, delimitador);
						string replacement = inicio.substr(delimitador);
						diccionario.insert(make_pair(token, replacement));
						}
{expdirectiva}         {string enunciado = strdup(yytext);
						string token = enunciado.substr(1);
						  
					 	if(diccionario.find(token) == diccionario.end()){
						     	ReportError::InvalidDirective(lineas);
					  	}else{
					    	string valor = diccionario.find(token)->second;
					      	printf("%s", valor.substr(1).c_str());
						  	}
						}

{errorDirectiva}     {ReportError::InvalidDirective(lineas); }

"\n"                  {lineas++;
		   			   string cadena = strdup(yytext);
		    		   printf("%s",cadena.c_str());  }

.	{
		string cadena = strdup(yytext);
		printf("%s", cadena.c_str());
	}



%%