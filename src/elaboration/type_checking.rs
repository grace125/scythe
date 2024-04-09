use super::*;


pub fn check(ctx: &mut Context, env: &mut Environment, t: Term, mut ty: Value) -> Result<Term, ElaborationError> {
    println!("CHECK {:?}", t);
    println!("CHECK TYPE {:?}", ty);
    match t {
        Term::Generic(x) => {
            ctx.insert_type(x, ty);
            Ok(Term::Generic(x))
        },
        Term::Func(patt, body) => {
            let g = GenericValue::new();
            let Value::FuncType(mut term_env, func_patt, func_arg_type, body_type) = ty else {
                todo!() // Improve error
            };

            term_env.start_scope();
            
            check_and_bind(ctx, env, func_patt, g.into(), *func_arg_type.clone())?;
            let body_type = evaluate(ctx, &mut term_env, *body_type)?;

            term_env.end_scope();

            env.start_scope();

            check_and_bind(ctx, env, patt.clone(), g.into(), *func_arg_type)?;
            let body = check(ctx, env, *body, body_type)?;

            env.end_scope();

            Ok(Term::Func(patt, Box::new(body)))
        },
        _ => {
            let (t_prime, mut ty_prime) = infer(ctx, env, t)?;
            if def_equal(ctx, env, &mut ty, &mut ty_prime)? {
                Ok(t_prime)
            }
            else {
                unimplemented!() // TODO: generate appropriate error message
            }
        }
    }
}

pub fn infer(ctx: &mut Context, env: &mut Environment, t: Term) -> Result<(Term, Value), ElaborationError> {
    println!("INFER {:?}", t);
    match t {
        Term::Generic(x) => {
            Ok((x.into(), ctx.get_type(x)?))
        },
        Term::ExternalFunc(ext_func) => {
            let ty = Value::FuncType(
                env.clone_for_closure(), 
                ext_func.pattern.clone(), 
                ext_func.arg_type.clone(), 
                ext_func.body_type.clone()
            );
            Ok((Term::ExternalFunc(ext_func), ty))
        }
        Term::Func(arg, body) => {
            env.start_scope();

            let (arg, arg_type) = infer_pattern(ctx, env, arg)?;

            let g = GenericValue::new().into();

            check_and_bind(ctx, env, arg.clone(), g, arg_type.clone())?;

            let (body, body_type) = infer(ctx, env, *body)?;

            let body_type = quote(ctx, env, body_type)?;

            env.end_scope();

            Ok((
                Term::Func(arg.clone(), Box::new(body)), 
                Value::FuncType(env.clone_for_closure(), arg, Box::new(arg_type), Box::new(body_type))
            ))
        },
        Term::FuncType(type_patt, arg_type, body_type) => {
            let g = GenericValue::new();

            env.start_scope();
            ctx.start_scope();
            
            let arg_type = check(ctx, env, *arg_type, Value::Type)?;
            let arg_type_value = evaluate(ctx, env, arg_type.clone())?;

            check_and_bind(ctx, env, type_patt.clone(), g.into(), arg_type_value)?;
            let body_type = check(ctx, env, *body_type, Value::Type)?;
            let pi = Term::FuncType(type_patt, Box::new(arg_type), Box::new(body_type));

            ctx.end_scope();
            env.end_scope();

            Ok((pi, Value::Type))
        },
        Term::Call(f, x) => {
            env.start_scope(); // TODO: is this necessary?
            
            let (f, f_type) = infer(ctx, env, *f)?;
            
            let Value::FuncType(mut ft_env, ft_patt, ft_arg_type, ft_body_type) = f_type else { 
                todo!() // TODO: Improve error
            };

            let x = check(ctx, env, *x, *ft_arg_type.clone())?;

            let v = evaluate(ctx, env, x.clone())?;

            ft_env.start_scope();

            let g = GenericValue::new();
            ft_env.insert_substitution(g, v);
            

            check_and_bind(ctx, &mut ft_env, ft_patt, g.into(), *ft_arg_type)?;

            let ft_body_type = evaluate(ctx, &mut ft_env, *ft_body_type)?;

            ft_env.end_scope();
            env.end_scope();

            Ok((
                Term::Call(Box::new(f), Box::new(x)),
                ft_body_type
            ))

        },
        Term::NatNum(n1) => Ok((Term::NatNum(n1), Value::Nat)),
        Term::Nat => Ok((Term::Nat, Value::Type)),
        Term::EmptyTuple => Ok((Term::EmptyTuple, Value::Unit)),
        Term::Unit => Ok((Term::Unit, Value::Type)),
        Term::Type => Ok((Term::Type, Value::Type)),
    } 
}

#[cfg(test)]
pub mod tests {
    use super::{*, super::tests::*};

    pub fn surface_check(s: SurfaceTerm, ty: SurfaceTerm) -> Result<Term, ElaborationError> {
        let mut ctx = Context::empty();
        surface_check_with_context(&mut ctx, s, ty)
    }

    pub fn surface_check_with_context(ctx: &mut Context, s: SurfaceTerm, ty: SurfaceTerm) -> Result<Term, ElaborationError> {
        let env = &mut ctx.global_environment();

        let term_ty = to_core(ctx, ty).unwrap();
        let term = to_core(ctx, s).unwrap();

        let term_ty = evaluate(ctx, env, term_ty).unwrap();
        println!("Checking starts here.");
        check(ctx, env, term, term_ty)
    }

    // pub fn surface_infer(s: SurfaceTerm) -> Result<(Term, Value), ElaborationError> {
    //     let mut ctx = Context::default();
    //     surface_infer_with_context(&mut ctx, s)
    // }

    pub fn surface_infer_with_context(ctx: &mut Context, s: SurfaceTerm) -> Result<(Term, Value), ElaborationError> {
        let env = &mut ctx.global_environment();

        let term = to_core(ctx, s).unwrap();
        println!("Inferrence starts here.");
        infer(ctx, env, term)
    }

    #[test]
    fn check_empty_tuple() {
        surface_check(EMPTY_TUPLE, UNIT).unwrap();
    }

    #[test]
    fn check_unit() {
        surface_check(UNIT, TYPE).unwrap();
    }

    #[test]
    fn check_type() {
        surface_check(TYPE, TYPE).unwrap();
    }

    #[test]
    fn check_func_1() {
        surface_check(
            func(pattern::var("x"), var("x")), 
            func_type(pattern::var("x"), UNIT, UNIT)
        ).unwrap();
    }

    #[test]
    fn infer_func_1() {
        let ctx = &mut Context::empty();
        let env = &mut ctx.global_environment();

        let (_, mut ty) = surface_infer_with_context(ctx, func(pattern::annot(pattern::var("x"), UNIT), var("x"))).unwrap();

        let mut ty_prime = surface_evaluate_with_context(ctx, func_type(pattern::var("x"), UNIT, UNIT)).unwrap();

        assert!(def_equal(ctx, env, &mut ty, &mut ty_prime).unwrap());
    }

    #[test]
    fn infer_empty_tuple() {
        let ctx = &mut Context::empty();
        let env = &mut ctx.global_environment();

        let (_, mut ty) = surface_infer_with_context(ctx, EMPTY_TUPLE).unwrap();

        assert!(def_equal(ctx, env, &mut ty, &mut Value::Unit).unwrap())
    }
}