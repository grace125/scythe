use super::*;

pub fn evaluate(ctx: &mut Context, env: &mut Environment, term: Term) -> Result<Value, ElaborationError> {
    match term {
        Term::Generic(term) => evaluate_neutral_value(ctx, env, env.try_get_binding(term)?.into()),
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
        Term::Tuple(l, r) => {
            let left = evaluate(ctx, env, *l)?;
            let right = evaluate(ctx, env, *r)?;
            Ok(evaluate_tuple(ctx, env, left, right))
        },
        Term::TupleType(patt, l_type, r_type) => {
            let l_type = Box::new(evaluate(ctx, env, *l_type)?);
            let g = GenericValue::new();

            env.start_scope();

            bind(ctx, env, &patt, g.into())?;
            let r_type_eval = evaluate(ctx, env, *r_type)?;
            let r_type = Box::new(quote(ctx, env, r_type_eval)?);

            env.end_scope();

            Ok(Value::TupleType(patt, l_type, r_type))
        }
        Term::NatNum(n) => Ok(Value::NatNum(n)),
        Term::StrLiteral(s) => Ok(Value::StrLiteral(s)),
        Term::EmptyTuple => Ok(Value::EmptyTuple),
        Term::Unit => Ok(Value::Unit),
        Term::Nat => Ok(Value::Nat),
        Term::Str => Ok(Value::Str),
        Term::Type => Ok(Value::Type),
        
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
        },
        NeutralValue::TupleLeft(l, r) => {
            let l = evaluate_neutral_value(ctx, env, *l)?;
            let r = if let Value::Neutral(n) = *r { evaluate_neutral_value(ctx, env, n)? } else { *r };
            Ok(evaluate_tuple(ctx, env, l, r))
        },
        NeutralValue::TupleRight(l, r) => {
            let r = evaluate_neutral_value(ctx, env, *r)?;
            Ok(evaluate_tuple(ctx, env, *l, r))
        },
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

fn evaluate_tuple(_ctx: &mut Context, _env: &mut Environment, left: Value, right: Value) -> Value {
    match (left, right) {
        (Value::Neutral(n_left), right) => NeutralValue::TupleLeft(Box::new(n_left), Box::new(right)).into(),
        (left, Value::Neutral(n_right)) => NeutralValue::TupleRight(Box::new(left), Box::new(n_right)).into(),
        (left, right) => Value::Tuple(Box::new(left), Box::new(right))
    }
}

pub fn quote(ctx: &mut Context, env: &mut Environment, value: Value) -> Result<Term, ElaborationError> { 
    match value {
        Value::Neutral(NeutralValue::Generic(g)) => env.try_get_unbinding(g).map(|x| x.into()),
        Value::Neutral(NeutralValue::Call(n, v)) => Ok(Term::Call(
            Box::new(quote(ctx, env, (*n).into())?), 
            Box::new(quote(ctx, env, *v)?)
        )),
        Value::Neutral(NeutralValue::ExternalCall(ext_call, n)) => Ok(Term::Call(
            Box::new(quote(ctx, env, Value::ExternalFunc(*ext_call))?), 
            Box::new(quote(ctx, env, (*n).into())?)
        )),
        Value::Neutral(NeutralValue::TupleLeft(l, r)) => Ok(Term::Tuple(
            Box::new(quote(ctx, env, Value::Neutral(*l))?), 
            Box::new(quote(ctx, env, *r)?), 
        )),
        Value::Neutral(NeutralValue::TupleRight(l, r)) => Ok(Term::Tuple(
            Box::new(quote(ctx, env, *l)?), 
            Box::new(quote(ctx, env, Value::Neutral(*r))?), 
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
        Value::Tuple(l, r) => Ok(Term::Tuple(
            Box::new(quote(ctx, env, *l)?), 
            Box::new(quote(ctx, env, *r)?)
        )),
        Value::EmptyTuple => Ok(Term::EmptyTuple),
        Value::NatNum(n) => Ok(Term::NatNum(n)),
        Value::StrLiteral(s) => Ok(Term::StrLiteral(s)),

        Value::FuncType(mut func_env, arg, arg_type, body_type) => {
            let g = GenericValue::new();

            func_env.start_scope();
            env.start_scope();

            let arg_type_term = Box::new(quote(ctx, env, *arg_type)?);

            bind(ctx, &mut func_env, &arg, g.into())?;
            bind(ctx, env, &arg, g.into())?;

            let body_type_val = evaluate(ctx, &mut func_env, *body_type)?;
            let f = Term::FuncType(arg, arg_type_term, Box::new(quote(ctx, env, body_type_val)?));

            env.end_scope();
            func_env.end_scope();

            Ok(f)
        },
        Value::TupleType(patt, l_type, r_type) => Ok(Term::TupleType(patt, Box::new(quote(ctx, env, *l_type)?), r_type)),
        Value::Unit => Ok(Term::Unit),
        Value::Nat => Ok(Term::Nat),
        Value::Str => Ok(Term::Str),
    
        Value::Type => Ok(Term::Type),
    }
}


#[cfg(test)]
pub mod tests {
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
        let res = def_equal(ctx, env, &mut v1, &mut v2);
        println!("{:?}", res);
        assert!(res.unwrap());
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


    #[test]
    fn evaluate_tuple() {
        assert_evaluates_to_eq(
            tuple(EMPTY_TUPLE, call(func(pattern::var("x"), var("x")), nat(3u32))), 
            tuple(call(func(pattern::var("x"), EMPTY_TUPLE), EMPTY_TUPLE), nat(3u32))
        )
    }

    #[test]
    fn evaluate_tuple_type() {
        assert_evaluates_to_eq(
            tuple_type(pattern::IGNORE, UNIT, UNIT), 
            tuple_type(pattern::var("x"), UNIT, UNIT), 
        )
    }
}