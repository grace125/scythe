use super::*;

pub fn evaluate(ctx: &mut Context, env: &mut Environment, term: Term) -> Result<Value, ElaborationError> {
    println!("EVALUATE: {:?}\n", term);
    match term {
        Term::Generic(term) => evaluate_neutral_value(ctx, env, env.get_binding(term)?.into()),
        Term::Func(patt, body) => {
            let g = GenericValue::new();
            env.start_scope();
            
            bind(ctx, env, &patt, g.into())?;
            let body_eval = evaluate(ctx, env, *body)?;
            let body = Box::new(quote(ctx, env, body_eval)?);

            env.end_scope();

            Ok(Value::Func(env.clone_for_closure(), patt, body))
        },
        Term::ExternalFunc(ext_func) => Ok(Value::ExternalFunc(ext_func)),
        Term::FuncType(patt, arg_type, body_type) => {
            let arg_type = Box::new(evaluate(ctx, env, *arg_type)?);
            let g = GenericValue::new();

            env.start_scope();
            
            bind(ctx, env, &patt, g.into())?;
            let body_type_eval = evaluate(ctx, env, *body_type)?;
            let body_type = Box::new(quote(ctx, env, body_type_eval)?);

            env.end_scope();

            Ok(Value::FuncType(env.clone_for_closure(), patt, arg_type, body_type))
        },
        Term::Call(f, x) => {
            let f_val = evaluate(ctx, env, *f)?;
            let x_val = evaluate(ctx, env, *x)?;
            evaluate_call(ctx, env, f_val, Box::new(x_val))
        },
        // Term::BinaryTuple(a, b) => Ok(Value::BinaryTuple(Box::new(env.evaluate(*a)?), Box::new(env.evaluate(*b)?))),
        Term::NatNum(n) => Ok(Value::NatNum(n)),
        Term::EmptyTuple => Ok(Value::EmptyTuple),
        Term::Unit => Ok(Value::Unit),
        Term::Type => Ok(Value::Type),
        Term::Nat => Ok(Value::Nat)
    }
}

#[inline]
fn evaluate_neutral_value(ctx: &mut Context, env: &mut Environment, neutral: NeutralValue) -> Result<Value, ElaborationError> {
    match neutral {
        NeutralValue::Generic(g) => match env.get_substitution(g) {
            Some(v) => if let Value::Neutral(n) = v {
                evaluate_neutral_value(ctx, env, n)
            } else {
                Ok(v)
            }
            None => Ok(neutral.into()),
        },
        NeutralValue::Call(f, v) => evaluate_call(ctx, env, (*f).into(), v),
        NeutralValue::ExternalCall(mut ext_call, n) => match evaluate_neutral_value(ctx, env, *n)? {
            Value::Neutral(n) => Ok(Value::Neutral(NeutralValue::ExternalCall(ext_call, Box::new(n)))),
            v => Ok((*ext_call.func)(v)?)
        }
    }
}

#[inline]
fn evaluate_call(ctx: &mut Context, _env: &mut Environment, f: Value, v: Box<Value>) -> Result<Value, ElaborationError> {
    match f {
        Value::Neutral(n) => Ok(NeutralValue::Call(Box::new(n), v).into()),
        Value::Func(mut func_env, arg, body) => {
            func_env.start_scope();

            bind(ctx, &mut func_env, &arg, *v)?;
            let y = evaluate(ctx, &mut func_env, *body)?;

            func_env.end_scope();

            Ok(y)
        },
        Value::ExternalFunc(mut ext_func) => {
            if let Value::Neutral(n) = *v {
                Ok(Value::Neutral(NeutralValue::ExternalCall(Box::new(ext_func), Box::new(n))))
            }
            else {
                Ok((*ext_func.func)(*v)?)
            }
        }
        // INVARIANT: type-checking will guarantee that this case never happens
        _ => unreachable!()
    }
}

pub fn quote(ctx: &mut Context, env: &mut Environment, value: Value) -> Result<Term, ElaborationError> {  
    println!("QUOTE {:?}\n", &value);
    match value {
        Value::Neutral(NeutralValue::Generic(g)) => env.get_unbinding(g).map(|x| x.into()),
        Value::Neutral(NeutralValue::Call(n, v)) => Ok(Term::Call(
            Box::new(quote(ctx, env, (*n).into())?), 
            Box::new(quote(ctx, env, *v)?)
        )),
        Value::Neutral(NeutralValue::ExternalCall(ext_call, n)) => Ok(Term::Call(
            Box::new(quote(ctx, env, Value::ExternalFunc(*ext_call))?), 
            Box::new(quote(ctx, env, (*n).into())?)
        )),
        Value::Func(mut func_env, arg, body) => { // TODO: fix function quoting's exponential behaviour
            func_env.start_scope();
            env.start_scope();

            let g = GenericValue::new();

            bind(ctx, &mut func_env, &arg, g.into())?;
            bind(ctx, env, &arg, g.into())?;

            let body_val = evaluate(ctx, &mut func_env, *body)?;
            let f = Term::Func(arg, Box::new(quote(ctx, env, body_val)?));

            env.end_scope();
            func_env.end_scope();

            Ok(f)
        },
        Value::ExternalFunc(ext_func) => Ok(Term::ExternalFunc(ext_func)),
        Value::FuncType(mut func_env, arg, arg_type, body_type) => {
            func_env.start_scope();
            env.start_scope();

            let arg_type_term = Box::new(quote(ctx, env, *arg_type)?);

            let g = GenericValue::new();

            bind(ctx, &mut func_env, &arg, g.into())?;
            bind(ctx, env, &arg, g.into())?;

            let body_type_val = evaluate(ctx, &mut func_env, *body_type)?;
            let f = Term::FuncType(arg, arg_type_term, Box::new(quote(ctx, env, body_type_val)?));

            env.end_scope();
            func_env.end_scope();

            Ok(f)
        },
        Value::EmptyTuple => Ok(Term::EmptyTuple),
        Value::NatNum(n) => Ok(Term::NatNum(n)),

        Value::Unit => Ok(Term::Unit),
        Value::Type => Ok(Term::Type),
        Value::Nat => Ok(Term::Nat),
        
    }
}


#[cfg(test)]
mod tests {
    use super::{*, super::tests::*};

    pub fn assert_evaluates_to(s: SurfaceTerm, mut v: Value) {
        let ctx = &mut Context::empty();
        let env = &mut ctx.global_environment();
        let mut w = surface_evaluate_with_context(ctx, s).unwrap();
        assert!(def_equal(ctx, env, &mut v, &mut w).unwrap());
    }

    pub fn assert_evaluates_to_eq(s1: SurfaceTerm, s2: SurfaceTerm) {
        let ctx = &mut Context::empty();
        let env = &mut ctx.global_environment();
        let t1 = to_core(ctx, s1).unwrap();
        let t2 = to_core(ctx, s2).unwrap();
        let mut v1 = evaluate(ctx, env, t1).unwrap();
        let mut v2 = evaluate(ctx, env, t2).unwrap();
        assert!(def_equal(ctx, env, &mut v1, &mut v2).unwrap());
    }

    #[test]
    fn evaluate_empty_tuple() {
        assert_evaluates_to(SurfaceTerm::EmptyTuple, Value::EmptyTuple)
    }

    #[test]
    fn evaluate_unit() {
        assert_evaluates_to(SurfaceTerm::Generic("Unit".to_owned()), Value::Unit)
    }

    #[test]
    fn evaluate_type() {
        assert_evaluates_to(SurfaceTerm::Generic("Type".to_owned()), Value::Type)
    }

    #[test]
    fn evaluate_func() {
        assert_evaluates_to_eq(
            func(pattern::var("x"), func(pattern::var("x"), var("x"))), 
            func(pattern::var("y"), func(pattern::var("z"), var("z"))), 
        );
    }

    #[test]
    fn evaluate_func_type() {
        assert_evaluates_to_eq(
            func_type(
                pattern::var("x"), 
                UNIT, 
                UNIT
            ), 
            func_type(
                pattern::var("y"), 
                UNIT, 
                UNIT
            ), 
        );
    }

    #[test]
    fn evaluate_binary_func_type() {
        assert_evaluates_to_eq(
            func_type(pattern::var("x"), UNIT, func_type(pattern::var("y"), UNIT, UNIT)), 
            func_type(pattern::var("x"), UNIT, func_type(pattern::var("x"), UNIT, UNIT)), 
        )
    }

    #[test]
    fn evaluate_func_type_complex() {
        assert_evaluates_to_eq(
            func_type(
                pattern::var("x"), 
                call(func(pattern::var("x"), var("x")), TYPE), 
                var("x")
            ), 
            func_type(
                pattern::var("y"), 
                TYPE, 
                var("y")
            ), 
        );
    }

    #[test]
    fn evaluate_call() {
        assert_evaluates_to(
            call(func(pattern::var("x"), EMPTY_TUPLE), EMPTY_TUPLE), 
            Value::EmptyTuple
        );
    }
    
    #[test]
    fn evaluate_neutral_call() {
        assert_evaluates_to_eq(
            func(pattern::var("f1"), call(var("f1"), EMPTY_TUPLE)), 
            func(pattern::var("f2"), call(var("f2"), EMPTY_TUPLE)), 
        );
    }
}