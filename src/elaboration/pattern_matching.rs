use super::*;

// XXX: Infer pattern
// XXX: Check pattern
// XXX: Bind pattern

pub(crate) fn check_and_bind(ctx: &mut Context, env: &mut Environment, p: Pattern, v: Value, mut v_type: Value) -> Result<(), ElaborationError> {
    match p {
        Pattern::Ignore => Ok(()),
        Pattern::EmptyTuple => if def_equal(ctx, env, &mut v_type, &mut Value::Unit)? {
            Ok(())
        } else {
            Err(ElaborationError::ExpectedTypeFoundType(Pattern::EmptyTuple, Value::Unit, v_type))
        },
        Pattern::Annotation(p_inner, asserted_type) => {
            let asserted_type = check(ctx, env, *asserted_type, Value::Type)?;
            let mut asserted_type = evaluate(ctx, env, asserted_type)?;
            if def_equal(ctx, env, &mut v_type, &mut asserted_type)? {
                check_and_bind(ctx, env, *p_inner, v, asserted_type)
            }
            else {
                Err(ElaborationError::ExpectedTypeFoundType(*p_inner, asserted_type, v_type))
            }
        },
        Pattern::Generic(x) => Ok({ 
            ctx.insert_type(x, v_type);
            env.insert(x, v); 
        }),
    }
}

pub(crate) fn check_pattern(ctx: &mut Context, env: &mut Environment, p: Pattern, p_type: Value) -> Result<(), ElaborationError> {
    env.start_scope();

    let g = GenericValue::new().into();
    let res = check_and_bind(ctx, env, p, g, p_type);

    env.end_scope();

    res
}

pub(crate) fn bind(ctx: &mut Context, env: &mut Environment, p: &Pattern, v: Value) -> Result<(), ElaborationError> {
    match p {
        Pattern::Ignore => Ok(()),
        Pattern::EmptyTuple => Ok(()),
        Pattern::Annotation(p_inner, _) => bind(ctx, env, p_inner, v),
        Pattern::Generic(x) => Ok({
            env.insert(*x, v); 
        })
    }
}

pub(crate) fn infer_pattern(ctx: &mut Context, env: &mut Environment, p: Pattern) -> Result<(Pattern, Value), ElaborationError> {
    match p {
        Pattern::Ignore => panic!("Can't infer type of _"), // TODO: replace with proper error
        Pattern::EmptyTuple => Ok((Pattern::EmptyTuple, Value::Unit)),
        Pattern::Generic(x) => panic!("Can't infer type of {:?}", x), // TODO: replace with proper error
        Pattern::Annotation(p_inner, p_type) => {
            let p_type = check(ctx, env, *p_type, Value::Type)?;
            let p_type = evaluate(ctx, env, p_type)?;
            check_pattern(ctx, env, *p_inner.clone(), p_type.clone())?;
            Ok((*p_inner, p_type))
        },
    }
}