#include <iostream>
#include <cstdio>
#include <cstring>

#include "ast.hpp"
#include "symtab.hpp"
#include "primitive.hpp"
#include "assert.h"

// WRITEME: The default attribute propagation rule
#define default_rule(X) X

#include <typeinfo>

class Typecheck : public Visitor
{
  private:
    FILE* m_errorfile;
    SymTab* m_st;

    // The set of recognized errors
    enum errortype
    {
        no_main,
        nonvoid_main,
        dup_proc_name,
        dup_var_name,
        proc_undef,
        call_type_mismatch,
        narg_mismatch,
        expr_type_err,
        var_undef,
        ifpred_err,
        whilepred_err,
        incompat_assign,
        who_knows,
        ret_type_mismatch,
        array_index_error,
        no_array_var,
        arg_type_mismatch,
        expr_pointer_arithmetic_err,
        expr_abs_error,
        expr_addressof_error,
        invalid_deref
    };

    // Print the error to file and exit
    void t_error(errortype e, Attribute a)
    {
        fprintf(m_errorfile,"on line number %d, ", a.lineno);

        switch(e)
        {
            case no_main:
                fprintf(m_errorfile, "error: no main\n");
                exit(2);
            case nonvoid_main:
                fprintf(m_errorfile, "error: the Main procedure has arguments\n");
                exit(3);
            case dup_proc_name:
                fprintf(m_errorfile, "error: duplicate procedure names in same scope\n");
                exit(4);
            case dup_var_name:
                fprintf(m_errorfile, "error: duplicate variable names in same scope\n");
                exit(5);
            case proc_undef:
                fprintf(m_errorfile, "error: call to undefined procedure\n");
                exit(6);
            case var_undef:
                fprintf(m_errorfile, "error: undefined variable\n");
                exit(7);
            case narg_mismatch:
                fprintf(m_errorfile, "error: procedure call has different number of args than declartion\n");
                exit(8);
            case arg_type_mismatch:
                fprintf(m_errorfile, "error: argument type mismatch\n");
                exit(9);
            case ret_type_mismatch:
                fprintf(m_errorfile, "error: type mismatch in return statement\n");
                exit(10);
            case call_type_mismatch:
                fprintf(m_errorfile, "error: type mismatch in procedure call args\n");
                exit(11);
            case ifpred_err:
                fprintf(m_errorfile, "error: predicate of if statement is not boolean\n");
                exit(12);
            case whilepred_err:
                fprintf(m_errorfile, "error: predicate of while statement is not boolean\n");
                exit(13);
            case array_index_error:
                fprintf(m_errorfile, "error: array index not integer\n");
                exit(14);
            case no_array_var:
                fprintf(m_errorfile, "error: attempt to index non-array variable\n");
                exit(15);
            case incompat_assign:
                fprintf(m_errorfile, "error: type of expr and var do not match in assignment\n");
                exit(16);
            case expr_type_err:
                fprintf(m_errorfile, "error: incompatible types used in expression\n");
                exit(17);
            case expr_abs_error:
                fprintf(m_errorfile, "error: absolute value can only be applied to integers and strings\n");
                exit(17);
            case expr_pointer_arithmetic_err:
                fprintf(m_errorfile, "error: invalid pointer arithmetic\n");
                exit(18);
            case expr_addressof_error:
                fprintf(m_errorfile, "error: AddressOf can only be applied to integers, chars, and indexed strings\n");
                exit(19);
            case invalid_deref:
                fprintf(m_errorfile, "error: Deref can only be applied to integer pointers and char pointers\n");
                exit(20);
            default:
                fprintf(m_errorfile, "error: no good reason\n");
                exit(21);
        }
    }

    // Helpers
    // WRITEME: You might want write some hepler functions.

    // Type Checking
    // WRITEME: You need to implement type-checking for this project

    // Check that there is one and only one main
    void check_for_one_main(ProgramImpl* p)
    {
        Symbol *s = m_st->lookup(strdup("main"));
        if(s == NULL) 
            this -> t_error(no_main, p -> m_attribute);
        
        if(s -> m_arg_type.size() > 0)
            this -> t_error(nonvoid_main,  p -> m_attribute);
    }

    // Create a symbol for the procedure and check there is none already
    // existing
    void add_proc_symbol(ProcImpl* p)
    {
        char *name; 
        Symbol *s;  
        
        name = strdup((p->m_symname)->spelling());
        s = new Symbol();
        s->m_basetype = p->m_type->m_attribute.m_basetype;

        if (!m_st->insert(name, s)){
            this->t_error(dup_proc_name,  p->m_attribute);
        }
    }

    // Add symbol table information for all the declarations following
    void add_decl_symbol(DeclImpl* p)
    {
        std::list<SymName_ptr>::iterator iter;
        char *name; 
        Symbol *s;  
        
        for (iter = p->m_symname_list->begin(); iter != p->m_symname_list->end(); ++iter) {
            name = strdup((*iter)->spelling());
            s = new Symbol();
            s->m_basetype = p->m_type->m_attribute.m_basetype;

            if (!m_st->insert(name, s)){
                this->t_error(dup_var_name,  p->m_attribute);
            }
        }
    }

    // Check that the return statement of a procedure has the appropriate type
    void check_proc(ProcImpl *p)
    {   
        Procedure_blockImpl *proc_body = dynamic_cast<Procedure_blockImpl*>(p->m_procedure_block);
        if(p->m_type->m_attribute.m_basetype != proc_body->m_return_stat->m_attribute.m_basetype){
            this->t_error(ret_type_mismatch, p->m_attribute);
        }
    }

    // Check that the declared return type is not an array
    void check_return(Return *p)
    {
        if(p->m_attribute.m_basetype != bt_string)
            this->t_error(ret_type_mismatch, p->m_attribute);
    }

    // Create a symbol for the procedure and check there is none already
    // existing
    void check_call(Call *p)
    {

    }

    // For checking that this expressions type is boolean used in if/else
    void check_pred_if(Expr* p)
    {
    }

    // For checking that this expressions type is boolean used in while
    void check_pred_while(Expr* p)
    {
    }

    void check_assignment(Assignment* p)
    {
    }

    void check_string_assignment(StringAssignment* p)
    {
    }

    void check_array_access(ArrayAccess* p)
    {
    }

    void check_array_element(ArrayElement* p)
    {
    }

    // For checking boolean operations(and, or ...)
    void checkset_boolexpr(Expr* parent, Expr* child1, Expr* child2)
    {
    }

    // For checking arithmetic expressions(plus, times, ...)
    void checkset_arithexpr(Expr* parent, Expr* child1, Expr* child2)
    {
    }

    // Called by plus and minus: in these cases we allow pointer arithmetics
    void checkset_arithexpr_or_pointer(Expr* parent, Expr* child1, Expr* child2)
    {
    }

    // For checking relational(less than , greater than, ...)
    void checkset_relationalexpr(Expr* parent, Expr* child1, Expr* child2)
    {
    }

    // For checking equality ops(equal, not equal)
    void checkset_equalityexpr(Expr* parent, Expr* child1, Expr* child2)
    {
    }

    // For checking not
    void checkset_not(Expr* parent, Expr* child)
    {
    }

    // For checking unary minus
    void checkset_uminus(Expr* parent, Expr* child)
    {
    }

    void checkset_absolute_value(Expr* parent, Expr* child)
    {
    }

    void checkset_addressof(Expr* parent, Lhs* child)
    {
    }

    void checkset_deref_expr(Deref* parent,Expr* child)
    {
    }

    // Check that if the right-hand side is an lhs, such as in case of
    // addressof
    void checkset_deref_lhs(DerefVariable* p)
    {
    }

    void checkset_variable(Variable* p)
    {
    }


  public:

    Typecheck(FILE* errorfile, SymTab* st) {
        m_errorfile = errorfile;
        m_st = st;
    }

    void visitProgramImpl(ProgramImpl* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);
    }

    void visitProcImpl(ProcImpl* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();

        add_proc_symbol(p);
        m_st -> open_scope();

        p->visit_children(this);

        // for each argument name, add the type to the arg list
        std::list<Decl_ptr>::iterator iter;
        for(iter = p->m_decl_list->begin(); iter != p->m_decl_list->end(); ++iter) {
            DeclImpl *decl = dynamic_cast<DeclImpl*>(*iter);
            add_decl_symbol(decl);
        }
        // descend into the implementation
        check_proc(p);        
        m_st->close_scope();
    }

    void visitCall(Call* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);
        check_call(p);
    }

    void visitNested_blockImpl(Nested_blockImpl* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);
    }

    void visitProcedure_blockImpl(Procedure_blockImpl* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);
    }

    void visitDeclImpl(DeclImpl* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);
        add_decl_symbol(p);
    }

    void visitAssignment(Assignment* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);
        check_assignment(p);
    }

    void visitStringAssignment(StringAssignment *p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);
        check_string_assignment(p);
    }

    void visitIdent(Ident* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);//hi:)
    }

    void visitReturn(Return* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);
        check_return(p);
    }

    void visitIfNoElse(IfNoElse* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);
        if(p->m_expr->m_attribute.m_basetype != bt_boolean)
            t_error(ifpred_err, p->m_attribute);
    }

    void visitIfWithElse(IfWithElse* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        if(p->m_expr->m_attribute.m_basetype != bt_boolean)
            t_error(ifpred_err, p->m_attribute);

    }

    void visitWhileLoop(WhileLoop* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        if(p->m_expr->m_attribute.m_basetype != bt_boolean)
            t_error(whilepred_err, p->m_attribute);
        
    }

    void visitCodeBlock(CodeBlock *p) 
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);
    }

    void visitTInteger(TInteger* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        p->m_attribute.m_basetype = bt_integer;
    }

    void visitTBoolean(TBoolean* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        p->m_attribute.m_basetype = bt_boolean;
    }

    void visitTCharacter(TCharacter* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        p->m_attribute.m_basetype = bt_char;
    }

    void visitTString(TString* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        p->m_attribute.m_basetype = bt_string;
    }

    void visitTCharPtr(TCharPtr* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        p->m_attribute.m_basetype = bt_charptr;

    }

    void visitTIntPtr(TIntPtr* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        p->m_attribute.m_basetype = bt_intptr;
    }

    void visitAnd(And* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        if((p->m_expr_1->m_attribute.m_basetype != bt_boolean)
            ||(p->m_expr_1->m_attribute.m_basetype != bt_boolean))
            t_error(expr_type_err,p->m_attribute);

        p->m_attribute.m_basetype = bt_boolean;
    }

    void visitDiv(Div* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        if((p->m_expr_1->m_attribute.m_basetype != bt_integer)
            ||(p->m_expr_1->m_attribute.m_basetype != bt_integer))
            t_error(expr_type_err,p->m_attribute);

        p->m_attribute.m_basetype = bt_integer;
    }

    void visitCompare(Compare* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        Basetype b1 = p->m_expr_1->m_attribute.m_basetype;
        Basetype b2 = p->m_expr_2->m_attribute.m_basetype;
        if(((b1 == bt_boolean)
        || (b1 == bt_integer)
        || (b1 == bt_char)
        || (b1 == bt_string)
        || (b1 == bt_charptr)
        || (b1 == bt_intptr)
        || (b1 == bt_ptr)) && (b1 == b2)){
            p->m_attribute.m_basetype = bt_boolean;
        }else if((b1 == bt_ptr) && ((b2 == bt_charptr)||(b2==bt_intptr))){
            p->m_attribute.m_basetype = bt_boolean;
        }else if((b2 == bt_ptr) && ((b1 == bt_charptr)||(b1==bt_intptr))){
            p->m_attribute.m_basetype = bt_boolean;
        }else{
            t_error(expr_type_err,p->m_attribute);
        }
    }

    void visitGt(Gt* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        if((p->m_expr_1->m_attribute.m_basetype != bt_integer)
            ||(p->m_expr_1->m_attribute.m_basetype != bt_integer))
            t_error(expr_type_err,p->m_attribute);

        p->m_attribute.m_basetype = bt_boolean;
    }

    void visitGteq(Gteq* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        if((p->m_expr_1->m_attribute.m_basetype != bt_integer)
            ||(p->m_expr_1->m_attribute.m_basetype != bt_integer))
            t_error(expr_type_err,p->m_attribute);

        p->m_attribute.m_basetype = bt_boolean;
    }

    void visitLt(Lt* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        if((p->m_expr_1->m_attribute.m_basetype != bt_integer)
            ||(p->m_expr_1->m_attribute.m_basetype != bt_integer))
            t_error(expr_type_err,p->m_attribute);

        p->m_attribute.m_basetype = bt_boolean;
    }

    void visitLteq(Lteq* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        if((p->m_expr_1->m_attribute.m_basetype != bt_integer)
            ||(p->m_expr_1->m_attribute.m_basetype != bt_integer))
            t_error(expr_type_err,p->m_attribute);

        p->m_attribute.m_basetype = bt_boolean;
    }

    void visitMinus(Minus* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        Basetype b1 = p->m_expr_1->m_attribute.m_basetype;
        Basetype b2 = p->m_expr_2->m_attribute.m_basetype;
        if((b1 == bt_integer && b2 == bt_integer) || 
            (b1 == bt_intptr && b2 == bt_integer)||
            (b1 == bt_charptr && b2 == bt_integer)||
            (b1 == bt_ptr && b2 == bt_integer))
            t_error(expr_type_err,p->m_attribute);

        p->m_attribute.m_basetype = bt_integer;
    }

    void visitNoteq(Noteq* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        Basetype b1 = p->m_expr_1->m_attribute.m_basetype;
        Basetype b2 = p->m_expr_2->m_attribute.m_basetype;
        if(((b1 == bt_boolean)
        || (b1 == bt_integer)
        || (b1 == bt_char)
        || (b1 == bt_string)
        || (b1 == bt_charptr)
        || (b1 == bt_intptr)
        || (b1 == bt_ptr)) && (b1 == b2)){
            p->m_attribute.m_basetype = bt_boolean;
        }else if((b1 == bt_ptr) && ((b2 == bt_charptr)||(b2==bt_intptr))){
            p->m_attribute.m_basetype = bt_boolean;
        }else if((b2 == bt_ptr) && ((b1 == bt_charptr)||(b1==bt_intptr))){
            p->m_attribute.m_basetype = bt_boolean;
        }else{
            t_error(expr_type_err,p->m_attribute);
        }
    }

    void visitOr(Or* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        if((p->m_expr_1->m_attribute.m_basetype != bt_boolean)
            ||(p->m_expr_1->m_attribute.m_basetype != bt_boolean))
            t_error(expr_type_err,p->m_attribute);

        p->m_attribute.m_basetype = bt_boolean;
    }

    void visitPlus(Plus* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        Basetype b1 = p->m_expr_1->m_attribute.m_basetype;
        Basetype b2 = p->m_expr_2->m_attribute.m_basetype;
        if((b1 == bt_integer) && (b2 == bt_integer)){
            p->m_attribute.m_basetype = bt_integer;
        }else if((b1 == bt_intptr) && (b2 == bt_integer)){
            p->m_attribute.m_basetype = bt_intptr;
        }else if((b1 == bt_charptr) && (b2 == bt_integer)){
            p->m_attribute.m_basetype = bt_charptr;
        }else if((b1 == bt_ptr) && (b2 == bt_integer)){
            p->m_attribute.m_basetype = bt_ptr;
        }else{
            t_error(expr_type_err,p->m_attribute);
        }

        
    }

    void visitTimes(Times* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        if((p->m_expr_1->m_attribute.m_basetype != bt_integer)
            ||(p->m_expr_1->m_attribute.m_basetype != bt_integer))
            t_error(expr_type_err,p->m_attribute);

        p->m_attribute.m_basetype = bt_integer;

    }

    void visitNot(Not* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);
        if(p->m_expr->m_attribute.m_basetype != bt_boolean)
            t_error(expr_type_err, p->m_attribute);
        p->m_attribute.m_basetype = bt_boolean;
    }

    void visitUminus(Uminus* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);
        if(p->m_expr->m_attribute.m_basetype != bt_integer)
            t_error(expr_type_err, p->m_attribute);
        p->m_attribute.m_basetype = bt_integer;

    }

    void visitArrayAccess(ArrayAccess* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        //Check the array ID has been declared and is of string type
        Symbol* s = m_st->lookup(p->m_symname->spelling());
        if(s == NULL)
            t_error(var_undef, p->m_attribute);
        if (s->m_basetype != bt_string)
            t_error(no_array_var,p->m_attribute);

        //Check the index is an integer
        if(p->m_expr->m_attribute.m_basetype != bt_integer)
            t_error(array_index_error, p->m_attribute);
    }

    void visitIntLit(IntLit* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);
        p -> m_attribute.m_basetype = bt_integer;
    }

    void visitCharLit(CharLit* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);
        p -> m_attribute.m_basetype = bt_char;
    }

    void visitBoolLit(BoolLit* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);
        p -> m_attribute.m_basetype = bt_boolean;
    }

    void visitNullLit(NullLit* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);
        p -> m_attribute.m_basetype = bt_ptr;
    }

    void visitAbsoluteValue(AbsoluteValue* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        if ((p->m_expr->m_attribute.m_basetype != bt_integer) && 
        (p->m_expr->m_attribute.m_basetype != bt_string))
            t_error(expr_abs_error,p->m_attribute);
        
        p -> m_attribute.m_basetype = bt_integer;
    }

    void visitAddressOf(AddressOf* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);
        
        if (p->m_lhs->m_attribute.m_basetype == bt_integer){
            p -> m_attribute.m_basetype = bt_intptr;
        }else if(p->m_lhs->m_attribute.m_basetype == bt_char){
            p -> m_attribute.m_basetype = bt_charptr;
        }else{
            t_error(expr_addressof_error,p->m_attribute);
        }
    }

    void visitVariable(Variable* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        Symbol* s = m_st->lookup(p->m_symname->spelling());
        if(s == NULL)
            t_error(var_undef, p->m_attribute);
        
    }

    void visitDeref(Deref* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        //check expression can be dereferenced
        if (p->m_expr->m_attribute.m_basetype == bt_intptr){
            p -> m_attribute.m_basetype = bt_integer;
        }else if(p->m_expr->m_attribute.m_basetype == bt_charptr){
            p -> m_attribute.m_basetype = bt_char;
        }else{
            t_error(invalid_deref,p->m_attribute);
        }
    }

    void visitDerefVariable(DerefVariable* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        //check the variable ID has been declared and is intptr or charptr
        Symbol* s = m_st->lookup(p->m_symname->spelling());
        if(s == NULL)
            t_error(var_undef, p->m_attribute);

        if (s->m_basetype == bt_intptr){
            p -> m_attribute.m_basetype = bt_integer;
        }else if(s->m_basetype == bt_charptr){
            p -> m_attribute.m_basetype = bt_char;
        }else{
            t_error(invalid_deref,p->m_attribute);
        }
        
    }

    void visitArrayElement(ArrayElement* p)
    {
        p->m_attribute.m_scope = m_st->get_scope();
	    p->visit_children(this);

        //Check the array ID has been declared and is of string type
        Symbol* s = m_st->lookup(p->m_symname->spelling());
        if(s == NULL)
            t_error(var_undef, p->m_attribute);
        if (s->m_basetype != bt_string)
            t_error(no_array_var,p->m_attribute);

        //Check the index is an integer
        if(p->m_expr->m_attribute.m_basetype != bt_integer)
            t_error(array_index_error, p->m_attribute);
    }

    // Special cases
    void visitPrimitive(Primitive* p) {}
    void visitSymName(SymName* p) {}
    void visitStringPrimitive(StringPrimitive* p) {}
};


void dopass_typecheck(Program_ptr ast, SymTab* st)
{
    Typecheck* typecheck = new Typecheck(stderr, st);
    ast->accept(typecheck); // Walk the tree with the visitor above
    delete typecheck;
}
