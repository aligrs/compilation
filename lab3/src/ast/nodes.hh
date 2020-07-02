// This file is automatically generated from nodes.yml
// ANY MODIFICATIONS MADE HERE WILL BE LOST

#ifndef NODES_HH
#define NODES_HH

#include <cassert>
#include <string>
#include <vector>

#include <boost/flyweight.hpp>
#include <boost/flyweight/no_locking.hpp>
#include <boost/optional.hpp>

#include "../parser/location.hh"

namespace llvm {
class Value;
} // namespace llvm

namespace ast {

inline namespace types {

using boost::optional;

typedef boost::flyweight<std::string, boost::flyweights::no_locking> Symbol;

using yy::location;

typedef enum { t_undef = 0, t_int, t_string, t_void } Type;
typedef enum {
  o_plus = 0,
  o_minus,
  o_times,
  o_divide,
  o_eq,
  o_neq,
  o_lt,
  o_le,
  o_gt,
  o_ge
} Operator;
const std::string operator_name[] = {"+",  "-", "*",  "/", "=",
                                     "<>", "<", "<=", ">", ">="};

class ASTVisitor {
public:
  virtual void visit(class IntegerLiteral &) = 0;
  virtual void visit(class StringLiteral &) = 0;
  virtual void visit(class BinaryOperator &) = 0;
  virtual void visit(class Sequence &) = 0;
  virtual void visit(class Let &) = 0;
  virtual void visit(class Identifier &) = 0;
  virtual void visit(class IfThenElse &) = 0;
  virtual void visit(class VarDecl &) = 0;
  virtual void visit(class FunDecl &) = 0;
  virtual void visit(class FunCall &) = 0;
  virtual void visit(class WhileLoop &) = 0;
  virtual void visit(class ForLoop &) = 0;
  virtual void visit(class Break &) = 0;
  virtual void visit(class Assign &) = 0;
};

class ConstASTVisitor {
public:
  virtual void visit(const class IntegerLiteral &) = 0;
  virtual void visit(const class StringLiteral &) = 0;
  virtual void visit(const class BinaryOperator &) = 0;
  virtual void visit(const class Sequence &) = 0;
  virtual void visit(const class Let &) = 0;
  virtual void visit(const class Identifier &) = 0;
  virtual void visit(const class IfThenElse &) = 0;
  virtual void visit(const class VarDecl &) = 0;
  virtual void visit(const class FunDecl &) = 0;
  virtual void visit(const class FunCall &) = 0;
  virtual void visit(const class WhileLoop &) = 0;
  virtual void visit(const class ForLoop &) = 0;
  virtual void visit(const class Break &) = 0;
  virtual void visit(const class Assign &) = 0;
};

class ConstASTValueVisitor {
public:
  virtual llvm::Value *visit(const class IntegerLiteral &) = 0;
  virtual llvm::Value *visit(const class StringLiteral &) = 0;
  virtual llvm::Value *visit(const class BinaryOperator &) = 0;
  virtual llvm::Value *visit(const class Sequence &) = 0;
  virtual llvm::Value *visit(const class Let &) = 0;
  virtual llvm::Value *visit(const class Identifier &) = 0;
  virtual llvm::Value *visit(const class IfThenElse &) = 0;
  virtual llvm::Value *visit(const class VarDecl &) = 0;
  virtual llvm::Value *visit(const class FunDecl &) = 0;
  virtual llvm::Value *visit(const class FunCall &) = 0;
  virtual llvm::Value *visit(const class WhileLoop &) = 0;
  virtual llvm::Value *visit(const class ForLoop &) = 0;
  virtual llvm::Value *visit(const class Break &) = 0;
  virtual llvm::Value *visit(const class Assign &) = 0;
};

class Node {

  // Private fields
  Type type = t_undef;

public:
  // Public fields
  const location loc;

  // Constructor
  Node(const location &_loc) : loc(_loc) {}

  // Destructor
  virtual ~Node() {}

  // Setter and getters for field `type'
  void set_type(Type _type) {
    assert(type == t_undef && _type != t_undef);
    type = _type;
  }
  Type &get_type() { return type; }
  const Type &get_type() const { return type; }

  // Acceptor method for visitors
  virtual void accept(ASTVisitor &visitor) = 0;
  virtual void accept(ConstASTVisitor &visitor) const = 0;
  virtual llvm::Value *accept(ConstASTValueVisitor &visitor) const = 0;
};

class Expr : public Node {
public:
  // Constructor
  Expr(const location &_loc) : Node(_loc) {}
};

class Decl : public Node {
public:
  // Public fields
  const Symbol name;
  int depth = -1;

  // Constructor
  Decl(const location &_loc, const Symbol &_name) : Node(_loc), name(_name) {}

  // Setter and getters for field `depth'
  void set_depth(int _depth) {
    assert(depth == -1 && _depth != -1);
    depth = _depth;
  }
  int &get_depth() { return depth; }
  const int &get_depth() const { return depth; }
};

class IntegerLiteral : public Expr {
public:
  // Public fields
  const int value;

  // Constructor
  IntegerLiteral(const location &_loc, const int &_value)
      : Expr(_loc), value(_value) {}

  // Acceptor method for visitors
  virtual void accept(ASTVisitor &visitor) { visitor.visit(*this); }
  virtual void accept(ConstASTVisitor &visitor) const { visitor.visit(*this); }
  virtual llvm::Value *accept(ConstASTValueVisitor &visitor) const {
    return visitor.visit(*this);
  }
};

class StringLiteral : public Expr {
public:
  // Public fields
  const Symbol value;

  // Constructor
  StringLiteral(const location &_loc, const Symbol &_value)
      : Expr(_loc), value(_value) {}

  // Acceptor method for visitors
  virtual void accept(ASTVisitor &visitor) { visitor.visit(*this); }
  virtual void accept(ConstASTVisitor &visitor) const { visitor.visit(*this); }
  virtual llvm::Value *accept(ConstASTValueVisitor &visitor) const {
    return visitor.visit(*this);
  }
};

class BinaryOperator : public Expr {

  // Private fields
  Expr *left;
  Expr *right;

public:
  // Public fields
  const Operator op;

  // Constructor
  BinaryOperator(const location &_loc, Expr *_left, Expr *_right,
                 const Operator &_op)
      : Expr(_loc), left(_left), right(_right), op(_op) {}

  // Destructor
  virtual ~BinaryOperator() {
    delete right;
    delete left;
  }

  // Getters for field `left'
  Expr &get_left() { return *left; }
  const Expr &get_left() const { return *left; }

  // Getters for field `right'
  Expr &get_right() { return *right; }
  const Expr &get_right() const { return *right; }

  // Acceptor method for visitors
  virtual void accept(ASTVisitor &visitor) { visitor.visit(*this); }
  virtual void accept(ConstASTVisitor &visitor) const { visitor.visit(*this); }
  virtual llvm::Value *accept(ConstASTValueVisitor &visitor) const {
    return visitor.visit(*this);
  }
};

class Sequence : public Expr {

  // Private fields
  std::vector<Expr *> exprs;

public:
  // Constructor
  Sequence(const location &_loc, const std::vector<Expr *> &_exprs)
      : Expr(_loc), exprs(_exprs) {}

  // Destructor
  virtual ~Sequence() {
    for (auto expr : exprs)
      delete expr;
  }

  // Getters for field `exprs'
  std::vector<Expr *> &get_exprs() { return exprs; }
  const std::vector<Expr *> &get_exprs() const { return exprs; }

  // Acceptor method for visitors
  virtual void accept(ASTVisitor &visitor) { visitor.visit(*this); }
  virtual void accept(ConstASTVisitor &visitor) const { visitor.visit(*this); }
  virtual llvm::Value *accept(ConstASTValueVisitor &visitor) const {
    return visitor.visit(*this);
  }
};

class Let : public Expr {

  // Private fields
  std::vector<Decl *> decls;
  Sequence *sequence;

public:
  // Constructor
  Let(const location &_loc, const std::vector<Decl *> &_decls,
      Sequence *_sequence)
      : Expr(_loc), decls(_decls), sequence(_sequence) {}

  // Destructor
  virtual ~Let() {
    delete sequence;
    for (auto decl : decls)
      delete decl;
  }

  // Getters for field `decls'
  std::vector<Decl *> &get_decls() { return decls; }
  const std::vector<Decl *> &get_decls() const { return decls; }

  // Getters for field `sequence'
  Sequence &get_sequence() { return *sequence; }
  const Sequence &get_sequence() const { return *sequence; }

  // Acceptor method for visitors
  virtual void accept(ASTVisitor &visitor) { visitor.visit(*this); }
  virtual void accept(ConstASTVisitor &visitor) const { visitor.visit(*this); }
  virtual llvm::Value *accept(ConstASTValueVisitor &visitor) const {
    return visitor.visit(*this);
  }
};

class Identifier : public Expr {

  // Private fields
  VarDecl *decl = nullptr;
  int depth = -1;

public:
  // Public fields
  const Symbol name;

  // Constructor
  Identifier(const location &_loc, const Symbol &_name)
      : Expr(_loc), name(_name) {}

  // Setter and getters for field `decl'
  void set_decl(VarDecl *_decl) {
    assert(!decl && _decl);
    decl = _decl;
  }
  optional<VarDecl &> get_decl() {
    if (!decl)
      return boost::none;
    return *decl;
  }
  const optional<const VarDecl &> get_decl() const {
    if (!decl)
      return boost::none;
    return *decl;
  }

  // Setter and getters for field `depth'
  void set_depth(int _depth) {
    assert(depth == -1 && _depth != -1);
    depth = _depth;
  }
  int &get_depth() { return depth; }
  const int &get_depth() const { return depth; }

  // Acceptor method for visitors
  virtual void accept(ASTVisitor &visitor) { visitor.visit(*this); }
  virtual void accept(ConstASTVisitor &visitor) const { visitor.visit(*this); }
  virtual llvm::Value *accept(ConstASTValueVisitor &visitor) const {
    return visitor.visit(*this);
  }
};

class IfThenElse : public Expr {

  // Private fields
  Expr *condition;
  Expr *then_part;
  Expr *else_part;

public:
  // Constructor
  IfThenElse(const location &_loc, Expr *_condition, Expr *_then_part,
             Expr *_else_part)
      : Expr(_loc), condition(_condition), then_part(_then_part),
        else_part(_else_part) {}

  // Destructor
  virtual ~IfThenElse() {
    delete else_part;
    delete then_part;
    delete condition;
  }

  // Getters for field `condition'
  Expr &get_condition() { return *condition; }
  const Expr &get_condition() const { return *condition; }

  // Getters for field `then_part'
  Expr &get_then_part() { return *then_part; }
  const Expr &get_then_part() const { return *then_part; }

  // Getters for field `else_part'
  Expr &get_else_part() { return *else_part; }
  const Expr &get_else_part() const { return *else_part; }

  // Acceptor method for visitors
  virtual void accept(ASTVisitor &visitor) { visitor.visit(*this); }
  virtual void accept(ConstASTVisitor &visitor) const { visitor.visit(*this); }
  virtual llvm::Value *accept(ConstASTValueVisitor &visitor) const {
    return visitor.visit(*this);
  }
};

class VarDecl : public Decl {

  // Private fields
  Expr *expr;
  bool escapes = false;

public:
  // Public fields
  const optional<Symbol> type_name;
  const bool read_only;

  // Constructor
  VarDecl(const location &_loc, const Symbol &_name, Expr *_expr,
          const optional<Symbol> &_type_name, const bool &_read_only = false)
      : Decl(_loc, _name), expr(_expr), type_name(_type_name),
        read_only(_read_only) {}

  // Destructor
  virtual ~VarDecl() { delete expr; }

  // Getters for field `expr'
  optional<Expr &> get_expr() {
    if (!expr)
      return boost::none;
    return *expr;
  }
  const optional<const Expr &> get_expr() const {
    if (!expr)
      return boost::none;
    return *expr;
  }

  // Setter and getters for field `escapes'
  void set_escapes() { escapes = true; }
  bool &get_escapes() { return escapes; }
  const bool &get_escapes() const { return escapes; }

  // Acceptor method for visitors
  virtual void accept(ASTVisitor &visitor) { visitor.visit(*this); }
  virtual void accept(ConstASTVisitor &visitor) const { visitor.visit(*this); }
  virtual llvm::Value *accept(ConstASTValueVisitor &visitor) const {
    return visitor.visit(*this);
  }
};

class FunDecl : public Decl {

  // Private fields
  std::vector<VarDecl *> params;
  Expr *expr;
  Symbol external_name = Symbol();
  FunDecl *parent = nullptr;
  std::vector<VarDecl *> escaping_decls = std::vector<VarDecl *>();

public:
  // Public fields
  const optional<Symbol> type_name;
  const bool is_external;

  // Constructor
  FunDecl(const location &_loc, const Symbol &_name,
          const std::vector<VarDecl *> &_params, Expr *_expr,
          const optional<Symbol> &_type_name, const bool &_is_external = false)
      : Decl(_loc, _name), params(_params), expr(_expr), type_name(_type_name),
        is_external(_is_external) {}

  // Destructor
  virtual ~FunDecl() {
    delete expr;
    for (auto param : params)
      delete param;
  }

  // Getters for field `params'
  std::vector<VarDecl *> &get_params() { return params; }
  const std::vector<VarDecl *> &get_params() const { return params; }

  // Getters for field `expr'
  optional<Expr &> get_expr() {
    if (!expr)
      return boost::none;
    return *expr;
  }
  const optional<const Expr &> get_expr() const {
    if (!expr)
      return boost::none;
    return *expr;
  }

  // Setter and getters for field `external_name'
  void set_external_name(Symbol _external_name) {
    assert(external_name == Symbol() && _external_name != Symbol());
    external_name = _external_name;
  }
  Symbol &get_external_name() { return external_name; }
  const Symbol &get_external_name() const { return external_name; }

  // Setter and getters for field `parent'
  void set_parent(FunDecl *_parent) {
    assert(!parent && _parent);
    parent = _parent;
  }
  optional<FunDecl &> get_parent() {
    if (!parent)
      return boost::none;
    return *parent;
  }
  const optional<const FunDecl &> get_parent() const {
    if (!parent)
      return boost::none;
    return *parent;
  }

  // Getters for field `escaping_decls'
  std::vector<VarDecl *> &get_escaping_decls() { return escaping_decls; }
  const std::vector<VarDecl *> &get_escaping_decls() const {
    return escaping_decls;
  }

  // Acceptor method for visitors
  virtual void accept(ASTVisitor &visitor) { visitor.visit(*this); }
  virtual void accept(ConstASTVisitor &visitor) const { visitor.visit(*this); }
  virtual llvm::Value *accept(ConstASTValueVisitor &visitor) const {
    return visitor.visit(*this);
  }
};

class FunCall : public Expr {

  // Private fields
  std::vector<Expr *> args;
  FunDecl *decl = nullptr;
  int depth = -1;

public:
  // Public fields
  const Symbol func_name;

  // Constructor
  FunCall(const location &_loc, const std::vector<Expr *> &_args,
          const Symbol &_func_name)
      : Expr(_loc), args(_args), func_name(_func_name) {}

  // Destructor
  virtual ~FunCall() {
    for (auto arg : args)
      delete arg;
  }

  // Getters for field `args'
  std::vector<Expr *> &get_args() { return args; }
  const std::vector<Expr *> &get_args() const { return args; }

  // Setter and getters for field `decl'
  void set_decl(FunDecl *_decl) {
    assert(!decl && _decl);
    decl = _decl;
  }
  optional<FunDecl &> get_decl() {
    if (!decl)
      return boost::none;
    return *decl;
  }
  const optional<const FunDecl &> get_decl() const {
    if (!decl)
      return boost::none;
    return *decl;
  }

  // Setter and getters for field `depth'
  void set_depth(int _depth) {
    assert(depth == -1 && _depth != -1);
    depth = _depth;
  }
  int &get_depth() { return depth; }
  const int &get_depth() const { return depth; }

  // Acceptor method for visitors
  virtual void accept(ASTVisitor &visitor) { visitor.visit(*this); }
  virtual void accept(ConstASTVisitor &visitor) const { visitor.visit(*this); }
  virtual llvm::Value *accept(ConstASTValueVisitor &visitor) const {
    return visitor.visit(*this);
  }
};

class Loop : public Expr {
public:
  // Constructor
  Loop(const location &_loc) : Expr(_loc) {}
};

class WhileLoop : public Loop {

  // Private fields
  Expr *condition;
  Expr *body;

public:
  // Constructor
  WhileLoop(const location &_loc, Expr *_condition, Expr *_body)
      : Loop(_loc), condition(_condition), body(_body) {}

  // Destructor
  virtual ~WhileLoop() {
    delete body;
    delete condition;
  }

  // Getters for field `condition'
  Expr &get_condition() { return *condition; }
  const Expr &get_condition() const { return *condition; }

  // Getters for field `body'
  Expr &get_body() { return *body; }
  const Expr &get_body() const { return *body; }

  // Acceptor method for visitors
  virtual void accept(ASTVisitor &visitor) { visitor.visit(*this); }
  virtual void accept(ConstASTVisitor &visitor) const { visitor.visit(*this); }
  virtual llvm::Value *accept(ConstASTValueVisitor &visitor) const {
    return visitor.visit(*this);
  }
};

class ForLoop : public Loop {

  // Private fields
  VarDecl *variable;
  Expr *high;
  Expr *body;

public:
  // Constructor
  ForLoop(const location &_loc, VarDecl *_variable, Expr *_high, Expr *_body)
      : Loop(_loc), variable(_variable), high(_high), body(_body) {}

  // Destructor
  virtual ~ForLoop() {
    delete body;
    delete high;
    delete variable;
  }

  // Getters for field `variable'
  VarDecl &get_variable() { return *variable; }
  const VarDecl &get_variable() const { return *variable; }

  // Getters for field `high'
  Expr &get_high() { return *high; }
  const Expr &get_high() const { return *high; }

  // Getters for field `body'
  Expr &get_body() { return *body; }
  const Expr &get_body() const { return *body; }

  // Acceptor method for visitors
  virtual void accept(ASTVisitor &visitor) { visitor.visit(*this); }
  virtual void accept(ConstASTVisitor &visitor) const { visitor.visit(*this); }
  virtual llvm::Value *accept(ConstASTValueVisitor &visitor) const {
    return visitor.visit(*this);
  }
};

class Break : public Expr {

  // Private fields
  Loop *loop = nullptr;

public:
  // Constructor
  Break(const location &_loc) : Expr(_loc) {}

  // Setter and getters for field `loop'
  void set_loop(Loop *_loop) {
    assert(!loop && _loop);
    loop = _loop;
  }
  Loop &get_loop() { return *loop; }
  const Loop &get_loop() const { return *loop; }

  // Acceptor method for visitors
  virtual void accept(ASTVisitor &visitor) { visitor.visit(*this); }
  virtual void accept(ConstASTVisitor &visitor) const { visitor.visit(*this); }
  virtual llvm::Value *accept(ConstASTValueVisitor &visitor) const {
    return visitor.visit(*this);
  }
};

class Assign : public Expr {

  // Private fields
  Identifier *lhs;
  Expr *rhs;

public:
  // Constructor
  Assign(const location &_loc, Identifier *_lhs, Expr *_rhs)
      : Expr(_loc), lhs(_lhs), rhs(_rhs) {}

  // Destructor
  virtual ~Assign() {
    delete rhs;
    delete lhs;
  }

  // Getters for field `lhs'
  Identifier &get_lhs() { return *lhs; }
  const Identifier &get_lhs() const { return *lhs; }

  // Getters for field `rhs'
  Expr &get_rhs() { return *rhs; }
  const Expr &get_rhs() const { return *rhs; }

  // Acceptor method for visitors
  virtual void accept(ASTVisitor &visitor) { visitor.visit(*this); }
  virtual void accept(ConstASTVisitor &visitor) const { visitor.visit(*this); }
  virtual llvm::Value *accept(ConstASTValueVisitor &visitor) const {
    return visitor.visit(*this);
  }
};

} // inline namespace types

} // namespace ast

#endif // NODES_HH
