#header
<<
#include <string>
#include <iostream>
#include <map>

using namespace std;

#define toInteger(s) atoi(s.c_str())

// struct to store information about tokens
typedef struct {
  string kind;
  string text;
} Attrib;



// function to fill token information (predeclaration)
void zzcr_attr(Attrib *attr, int type, char *text);

// fields for AST nodes
#define AST_FIELDS string kind; string text;
#include "ast.h"

// macro to create a new AST node (and function predeclaration)
#define zzcr_ast(as,attr,ttype,textt) as=createASTnode(attr,ttype,textt)
AST* createASTnode(Attrib* attr,int ttype, char *textt);
>>

<<
#include <cstdlib>
#include <cmath>
#include <vector>
typedef vector<int> Row;
typedef vector<Row> Matrix;
typedef struct{
  int h;
  int w;
  int x;
  int y;
} block;

typedef struct{
  int h,w;
  map<string,block*> blocks;
  Matrix heights;
} Graella;

//global structures
AST *root;
Graella g;


// function to fill token information
void zzcr_attr(Attrib *attr, int type, char *text) {
  switch(type) {
    case VAR:
      attr->text=text;
      attr->kind="VAR";
      break;
    case CMP_INT:
      attr->text=text;
      attr->kind="CMP_INT";
      break;
    case CMP:
      attr->text=text;
      attr->kind="CMP";
      break;
    case OP:
      attr->text=text;
      attr->kind="OP";
      break;
    case NUM:
      attr->text=text;
      attr->kind="NUM";
      break;
    case ASSIG:
      attr->text=text;
      attr->kind="ASSIG";
      break;
    default:
        attr->kind = text;
        attr->text = "";
  }
}

// function to create a new AST node
AST* createASTnode(Attrib* attr, int type, char* text) {
  AST* as = new AST;
  as->kind = attr->kind; 
  as->text = attr->text;
  as->right = NULL; 
  as->down = NULL;
  return as;
}


/// create a new "list" AST node with one element
AST* createASTlist(AST *child) {
 AST *as=new AST;
 as->kind="list";
 as->right=NULL;
 as->down=child;
 return as;
}

/// get nth child of a tree. Count starts at 0.
/// if no such child, returns NULL
AST* child(AST *a,int n) {
AST *c=a->down;
for (int i=0; c!=NULL && i<n; i++) c=c->right;
return c;
}



/// print AST, recursively, with indentation
void ASTPrintIndent(AST *a,string s)
{
  if (a==NULL) return;

  cout<<a->kind;
  if (a->text!="") cout<<"("<<a->text<<")";
  cout<<endl;

  AST *i = a->down;
  while (i!=NULL && i->right!=NULL) {
    cout<<s+"  \\__";
    ASTPrintIndent(i,s+"  |"+string(i->kind.size()+i->text.size(),' '));
    i=i->right;
  }
  
  if (i!=NULL) {
      cout<<s+"  \\__";
      ASTPrintIndent(i,s+"   "+string(i->kind.size()+i->text.size(),' '));
      i=i->right;
  }
}

/// print AST 
void ASTPrint(AST *a)
{
  while (a!=NULL) {
    cout<<" ";
    ASTPrintIndent(a,"");
    a=a->right;
  }
}

string HaskellPrintNExpr(AST *a)
{
  if (a->kind=="VAR") {
    return "Var \""+a->text+"\"";
  } else if (a->kind=="NUM") {
    return "Const "+a-> text;
  }else if (a->kind=="OP"){
    AST* f = child(a,0);
    AST* s = child(a,1);
    string op = "Times";
    if (a->text=="+"){
      op = "Plus";
    } else if (a->text == "-"){
      op = "Minus";
    }
    return op+" ("+HaskellPrintNExpr(f)+") ("+HaskellPrintNExpr(s)+")";

  }

}


string HaskellPrintBExpr(AST *a)
{
  if (a->kind=="CMP") {
    AST* f = child(a,0);
    AST* s = child(a,1);
    return a->text+" ("+HaskellPrintBExpr(f)+") ("+HaskellPrintBExpr(s)+")";
  } else if (a->kind=="NOT") {
    AST* f = child(a,0);
    return "NOT ("+HaskellPrintBExpr(f)+")";
  }else if (a->kind=="CMP_INT"){
    string op = "Gt";
    if (a->text=="="){
      op = "Eq";
    }
    AST* f = child(a,0);
    AST* s = child(a,1);
    return op+" ("+HaskellPrintNExpr(f)+") ("+HaskellPrintNExpr(s)+")";
  }

}
void HaskellPrintIndent(AST *a,string s)
{
  if (a==NULL) return;
  if (a->kind=="ASSIG"){
    AST* f = child(a,0);
    AST* s = child(a,1);
    cout<<"Assign \""<<f->text<<"\" ("<<HaskellPrintNExpr(s)<<")";
  } else if (a->kind=="list") {
    cout<<"Seq (";
    AST* i = a->down;
     while (i!=NULL && i->right!=NULL) {
        cout<<"(";
        HaskellPrintIndent(i,"");
        cout<<"):";
        i = i->right;
      }
      if (i!=NULL){
        cout<<"(";
        HaskellPrintIndent(i,"");
        cout<<"):[])";
      }
  } else if (a->kind=="INPUT"){
    AST* f = child(a,0);
    cout<<"Input \""<<f->text<<"\"";
  } else if (a->kind=="PRINT") {
    AST* f = child(a,0);
    cout<<"Print ("<<HaskellPrintNExpr(f)<<")";
  }else if (a->kind=="EMPTY") {
    AST* f = child(a,0);
    cout<<"Empty \""+f->text+"\""; 
  } else if (a->kind=="PUSH") {
    AST* f = child(a,0);
    AST* s = child(a,1);
    cout<<"Push \""+f->text+"\" ("+HaskellPrintNExpr(s)+")";
  } else if (a->kind=="POP") {
    AST* f = child(a,0);
    AST* s = child(a,1);
    cout<<"Pop \""+f->text+"\" \""+s->text+"\"";
  } else if (a->kind=="SIZE"){
    AST* f = child(a,0);
    AST* s = child(a,1);
    cout<<"Size \""+f->text+"\" \""+s->text+"\"";
  } else if (a->kind=="IF") {
    AST* f = child(a,0);
    AST* s = child(a,1);
    AST* t = child(a,2);
    cout<<"Cond ("<<HaskellPrintBExpr(f)<<") (";
    HaskellPrintIndent(s,"");
    cout<<") (";
    HaskellPrintIndent(t,"");
    cout<<")";
  } else if (a->kind=="WHILE") {
    AST* f = child(a,0);
    AST* s = child(a,1);
    cout<<"Loop ("<<HaskellPrintBExpr(f)<<") (";
    HaskellPrintIndent(s,"");
    cout<<")";
  }
}



/// print AST 
void HaskellPrint(AST *a)
{
  while (a!=NULL) {
    cout<<"";
    HaskellPrintIndent(a,"");
    a=a->right;
  }
}

int main() {
  root = NULL;
  ANTLR(compiler(&root), stdin);
  // ASTPrint(root);
  HaskellPrint(root);
}
>>

#lexclass START
#token ASSIG ":\="
#token INPUT "INPUT"
#token SIZE "SIZE"
#token EMPTY "EMPTY"
#token PUSH "PUSH"
#token POP "POP"
#token IF "IF"
#token THEN "THEN"
#token ELSE "ELSE"
#token END "END"
#token DO "DO"
#token CMP_INT "(>|\=)"
#token CMP "(AND|OR)"
#token NOT "NOT"
#token WHILE "WHILE"
#token PRINT "PRINT" 
#token OP "(\+|\-|\*)"
#token SPACE "[\ \n\t]" << zzskip();>>
#token NUM "[0-9]+"
#token VAR "[a-zA-Z][a-zA-Z0-9]*" 




//defs: INI_DEF^ VAR ops END_DEF!;
compiler: ops ;
ops: (assig|input|print|empty|push|pop|size|cond|my_while)* <<#0=createASTlist(_sibling);>>;
input: INPUT^ VAR;
print: PRINT^ numeric;
empty: EMPTY^ VAR;
push: PUSH^ VAR numeric;
pop: POP^ VAR VAR;
size: SIZE^ VAR VAR;
cond: IF^ boolean THEN! ops ELSE! ops END!;
my_while: WHILE^ boolean DO! ops END!;
assig: VAR ASSIG^ numeric;
numeric: b_n (OP^ numeric)*;
boolean:  s_b (CMP^ s_b)* ;
s_b: (NOT^|) cmp;
cmp: b_n CMP_INT^ b_n;
b_n: (NUM|VAR);

