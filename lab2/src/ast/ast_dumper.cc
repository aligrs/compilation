#include "ast_dumper.hh"
#include "../utils/errors.hh"

namespace ast {

void ASTDumper::visit(const IntegerLiteral &literal) {
  *ostream << literal.value;
}

void ASTDumper::visit(const StringLiteral &literal) {
  *ostream << '"';
  for (auto &c : static_cast<std::string>(literal.value)) {
    switch (c) {
    case '"':
      *ostream << "\\\"";
      break;
    case '\\':
      *ostream << "\\\\";
      break;
    case '\a':
      *ostream << "\\a";
      break;
    case '\b':
      *ostream << "\\b";
      break;
    case '\t':
      *ostream << "\\t";
      break;
    case '\n':
      *ostream << "\\n";
      break;
    case '\v':
      *ostream << "\\v";
      break;
    case '\f':
      *ostream << "\\f";
      break;
    case '\r':
      *ostream << "\\r";
      break;
    default:
      *ostream << c;
    }
  }
  *ostream << '"';
}

void ASTDumper::visit(const BinaryOperator &binop) {
  *ostream << '(';
  binop.get_left().accept(*this);
  *ostream << operator_name[binop.op];
  binop.get_right().accept(*this);
  *ostream << ')';
}

void ASTDumper::visit(const Sequence &seqExpr) {
  *ostream << "(";
  inc();
  const auto exprs = seqExpr.get_exprs();
  for (auto expr = exprs.cbegin(); expr != exprs.cend(); expr++) {
    if (expr != exprs.cbegin())
      *ostream << ';';
    nl();
    (*expr)->accept(*this);
  }
  dnl();
  *ostream << ")";
}

void ASTDumper::visit(const Let &let) {
  *ostream << "let";
  inc();
  for (auto decl : let.get_decls()) {
    nl();
    decl->accept(*this);
  }
  dnl();
  *ostream << "in";
  inc();
  const auto exprs = let.get_sequence().get_exprs();
  for (auto expr = exprs.cbegin(); expr != exprs.cend(); expr++) {
    if (expr != exprs.cbegin())
      *ostream << ';';
    nl();
    (*expr)->accept(*this);
  }
  dnl();
  *ostream << "end";
}

void ASTDumper::visit(const Identifier &id) {
  *ostream << id.name;
  if (verbose)
    if (auto decl = id.get_decl())
      if (int depth_diff = id.get_depth() - decl->get_depth())
        *ostream << "/*" << depth_diff << "*/";
}

void ASTDumper::visit(const IfThenElse &ite) {
  *ostream << "if ";
  inl();
  ite.get_condition().accept(*this);
  dnl();
  *ostream << " then ";
  inl();
  ite.get_then_part().accept(*this);
  dnl();
  *ostream << " else ";
  inl();
  ite.get_else_part().accept(*this);
  dec();
}

void ASTDumper::visit(const VarDecl &decl) {
  if (decl.get_expr())
    *ostream << "var ";
  *ostream << decl.name;
  if (verbose && decl.get_escapes())
    *ostream << "/*e*/";
  if (decl.type_name)
    *ostream << ": " << *decl.type_name;
  if (auto expr = decl.get_expr()) {
    *ostream << " := ";
    expr->accept(*this);
  }
}

void ASTDumper::visit(const FunDecl &decl) {
  *ostream << "function " << decl.name;
  if (verbose && decl.name != decl.get_external_name())
    *ostream << "/*" << decl.get_external_name() << "*/";
  *ostream << '(';
  auto params = decl.get_params();
  for (auto param = params.cbegin(); param != params.cend(); param++) {
    if (param != params.cbegin())
      *ostream << ", ";
    (*param)->accept(*this);
  }
  *ostream << ")";
  if (decl.type_name)
    *ostream << ": " << decl.type_name.get();
  *ostream << " = ";
  inl();
  decl.get_expr()->accept(*this);
  dec();
}

void ASTDumper::visit(const FunCall &call) {
  *ostream << call.func_name << '(';
  auto args = call.get_args();
  for (auto arg = args.cbegin(); arg != args.cend(); arg++) {
    if (arg != args.cbegin())
      *ostream << ", ";
    (*arg)->accept(*this);
  }
  *ostream << ')';
}

void ASTDumper::visit(const WhileLoop &loop) {
  *ostream << "while ";
  loop.get_condition().accept(*this);
  *ostream << " do";
  inl();
  loop.get_body().accept(*this);
  dec();
}

void ASTDumper::visit(const ForLoop &loop) {
  *ostream << "for " << loop.get_variable().name << " := ";
  loop.get_variable().get_expr()->accept(*this);
  *ostream << " to ";
  loop.get_high().accept(*this);
  *ostream << " do";
  inl();
  loop.get_body().accept(*this);
  dec();
}

void ASTDumper::visit(const Break &brk) { *ostream << "break"; }

void ASTDumper::visit(const Assign &assign) {
  assign.get_lhs().accept(*this);
  *ostream << " := ";
  assign.get_rhs().accept(*this);
}

int ASTevaluate::visit(const IntegerLiteral &literal){
  return literal.value;
}
int ASTevaluate::visit(const StringLiteral &literal){
  utils::error("erreur StringLiteral");
  return -1;
}
int ASTevaluate::visit(const BinaryOperator &binop){
  Operator op = binop.op;
  int right = binop.get_right().accept(*this);
  int left = binop.get_left().accept(*this);
  switch(op){
    case o_plus:
      return right + left;
    case o_minus:
      return right - left;
    case o_times:
      return right * left;
    case o_divide:
      return right / left;
    break;
    case o_eq:
      if(right == left) return 1;
      else return 0;
    case o_neq:
      if(right != left) return 1;
      else return 0;
    case o_lt:
      if(left < right) return 1;
      else return 0;
    case o_le:
      if(left <= right) return 1;
      else return 0;
    case o_gt:
      if(left > right) return 1;
      else return 0;
    case o_ge:
      if(left >= right) return 1;
      else return 0;
    default :
      utils::error("fatal error"); return -1;
  }
}
int ASTevaluate::visit(const Sequence &seqExpr){
  const auto exprs = seqExpr.get_exprs();
  if(exprs.empty()){
    utils::error("error expected, empty list, no expressions"); 
    return -1;
  }
  auto expr = exprs[exprs.size()-1];
  return (*expr).accept(*this);
}
int ASTevaluate::visit(const Let &let){
  utils::error("erreur Let");
  return -1;
}
int ASTevaluate::visit(const Identifier &id){
  utils::error("erreur Identifier");
  return -1;
}
int ASTevaluate::visit(const IfThenElse &ite){
  if(ite.get_condition().accept(*this))
    return ite.get_then_part().accept(*this);
  return ite.get_else_part().accept(*this);
}
int ASTevaluate::visit(const VarDecl &decl){
  utils::error("erreur varDecl");
  return -1;
}
int ASTevaluate::visit(const FunDecl &decl){
  utils::error("erreur FunDecl");
  return -1;
}
int ASTevaluate::visit(const FunCall &call){
  utils::error("erreur FunCall");
  return -1;
}
int ASTevaluate::visit(const WhileLoop &loop){
  utils::error("erreur WhileLoop");
  return -1;
}
int ASTevaluate::visit(const ForLoop &loop){
  utils::error("erreur ForLoop");
  return -1;
}
int ASTevaluate::visit(const Break &brk){
  utils::error("erreur Break");
  return -1;
}
int ASTevaluate::visit(const Assign &assign){
  utils::error("erreur Assign");
  return -1;
}

} // namespace ast
