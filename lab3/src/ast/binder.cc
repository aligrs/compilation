#include "binder.hh"
#include "../utils/errors.hh"
#include "../utils/nolocation.hh"

using utils::error;
using utils::non_fatal_error;

namespace ast {
namespace binder {

/* Returns the current scope */
scope_t &Binder::current_scope() { return scopes.back(); }

/* Pushes a new scope on the stack */
void Binder::push_scope() { scopes.push_back(scope_t()); depth += 1; }

/* Pops the current scope from the stack */
void Binder::pop_scope() { scopes.pop_back(); depth -= 1;}

/* Enter a declaration in the current scope. Raises an error if the declared name
 * is already defined */
void Binder::enter(Decl &decl) {
  scope_t &scope = current_scope();
  auto previous = scope.find(decl.name);
  if (previous != scope.end()) {
    non_fatal_error(decl.loc,
                    decl.name.get() + " is already defined in this scope");
    error(previous->second->loc, "previous declaration was here");
  }
  scope[decl.name] = &decl;
}

/* Finds the declaration for a given name. The scope stack is traversed
 * front to back starting from the current scope. The first matching
 * declaration is returned. Raises an error, if no declaration matches. */
Decl &Binder::find(const location loc, const Symbol &name) {
  for (auto scope = scopes.crbegin(); scope != scopes.crend(); scope++) {
    auto decl_entry = scope->find(name);
    if (decl_entry != scope->cend()) {
      return *decl_entry->second;
    }
  }
  error(loc, name.get() + " cannot be found in this scope");
}

Binder::Binder() : scopes() {
  /* Create the top-level scope */
  push_scope();

  /* Populate the top-level scope with all the primitive declarations */
  enter_primitive("print_err", "void", {"string"});
  enter_primitive("print", "void", {"string"});
  enter_primitive("print_int", "void", {"int"});
  enter_primitive("flush", "void", {});
  enter_primitive("getchar", "string", {});
  enter_primitive("ord", "int", {"string"});
  enter_primitive("chr", "string", {"int"});
  enter_primitive("size", "int", {"string"});
  enter_primitive("substring", "string", {"string", "int", "int"});
  enter_primitive("concat", "string", {"string", "string"});
  enter_primitive("strcmp", "int", {"string", "string"});
  enter_primitive("streq", "int", {"string", "string"});
  enter_primitive("not", "int", {"int"});
  enter_primitive("exit", "void", {"int"});
}

/* Declares a new primitive into the current scope*/
void Binder::enter_primitive(
    const std::string &name, const std::string &type_name,
    const std::vector<std::string> &argument_typenames) {
  std::vector<VarDecl *> args;
  int counter = 0;
  for (const std::string &tn : argument_typenames) {
    std::ostringstream argname;
    argname << "a_" << counter++;
    args.push_back(
        new VarDecl(utils::nl, Symbol(argname.str()), nullptr, Symbol(tn)));
  }

  FunDecl *fd = new FunDecl(utils::nl, Symbol(name), std::move(args), nullptr,
                            Symbol(type_name), true);
  fd->set_external_name(Symbol("__" + name));
  enter(*fd);
}

/* Binds a whole program. This method wraps the program inside a top-level main
 * function.  Then, it visits the programs with the Binder visitor; binding
 * each identifier to its declaration and computing depths.*/
FunDecl *Binder::analyze_program(Expr &root) {
  std::vector<VarDecl *> main_params;
  Sequence *const main_body = new Sequence(
      utils::nl,
      std::vector<Expr *>({&root, new IntegerLiteral(utils::nl, 0)}));
  FunDecl *const main = new FunDecl(utils::nl, Symbol("main"), main_params,
                                    main_body, Symbol("int"), true);
  main->accept(*this);
  return main;
}


void Binder::visit(IntegerLiteral &literal) {
}

void Binder::visit(StringLiteral &literal) {
}

void Binder::visit(BinaryOperator &op) {
  op.get_left().accept(*this);
  op.get_right().accept(*this);
}

void Binder::visit(Sequence &seq) {
  const auto exprs = seq.get_exprs();
  for (auto expr = exprs.cbegin(); expr != exprs.cend(); expr++) {
    (*expr)->accept(*this); 
  }
}
/*On doit pouvoir gerer le cas ou on declare deux fonction qui fond appelle
* l'une a l'autre 
* 
* function odd = even()
* function even = odd()
* 
* il faudrai les enter dans le let directement, car sinon sa ne marchera pas 
*/
void Binder::visit(Let &let) {
  //gestion des déclaration
  std::vector<Decl *> fdecl;
  push_scope();
  for (auto decl : let.get_decls()) {
    
    FunDecl* f = dynamic_cast<FunDecl*>(decl);
    
    if(f) { enter(*decl); fdecl.push_back(decl); }
    else {
      decl->accept(*this);
      while(!fdecl.empty()) {
        fdecl.back()->accept(*this);
        fdecl.pop_back();
      }
    }
    
  }
  while(!fdecl.empty()) {
    fdecl.back()->accept(*this);
    fdecl.pop_back();
  }


  /*Cette partie s'occupe de ce qui se trouve entre le in et end*/
  const auto exprs = let.get_sequence().get_exprs();
  for (auto expr = exprs.cbegin(); expr != exprs.cend(); expr++) {
    (*expr)->accept(*this);
  }
  pop_scope(); //On suprime un scope
}
/*on utilise un find ici*/
/*
* Animal *a;
* Tortue * t = dynamic_cast<Tortue *> ...
*/
void Binder::visit(Identifier &id) {
  id.set_depth(depth);

  Decl &decl = find(id.loc,id.name);
  VarDecl* vdecl = dynamic_cast<VarDecl *>(&decl);
  if(vdecl) id.set_decl(vdecl);
  else error("l'identifiant est non déclaré !");

  if(vdecl->get_depth() != id.get_depth()) vdecl->set_escapes();
}

void Binder::visit(IfThenElse &ite) {
  ite.get_condition().accept(*this);
  ite.get_then_part().accept(*this);
  ite.get_else_part().accept(*this);
}
/*on utilise enter a l'interieur de cette fonction*/
void Binder::visit(VarDecl &decl) {
  decl.set_depth(depth);

  enter(decl);
  if (auto expr = decl.get_expr()) {
    expr->accept(*this);
  }
}
/*on utilise enter a l'interieur de cette fonction*/
void Binder::visit(FunDecl &decl) {
  decl.set_depth(depth);

  push_scope();
  
  auto params = decl.get_params();
  for (auto param = params.cbegin(); param != params.cend(); param++) {
    (*param)->accept(*this);
  }
  decl.get_expr()->accept(*this);
  
  pop_scope();
}
/* on utilisera un find a l'interieur */
void Binder::visit(FunCall &call) {
  call.set_depth(depth);

  push_scope();
  
  Decl &decl = find(call.loc,call.func_name);
  FunDecl* fdecl = dynamic_cast<FunDecl *>(&decl);
  
  if(fdecl) call.set_decl(fdecl);
  else error("La fonction n'est pas déclaré !");
  
  auto args = call.get_args();
  for (auto arg = args.cbegin(); arg != args.cend(); arg++) {
    (*arg)->accept(*this);
  }
  
  pop_scope();
}

void Binder::visit(WhileLoop &loop) {
  loops.push_back(&loop);
  loop.get_condition().accept(*this);
  loop.get_body().accept(*this);
  loops.pop_back();
}

void Binder::visit(ForLoop &loop) {
  loops.push_back(&loop);
  loop.get_variable().get_expr()->accept(*this);
  loop.get_high().accept(*this);
  loop.get_body().accept(*this);
  loops.pop_back();
}

void Binder::visit(Break &b) {
  if(!loops.empty()) b.set_loop(loops.back());
  else error("break is used outside of a loop");
}

void Binder::visit(Assign &assign) {
  assign.get_lhs().accept(*this);
  assign.get_rhs().accept(*this);
}

} // namespace binder
} // namespace ast
