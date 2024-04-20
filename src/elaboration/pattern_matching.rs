use super::*;

// XXX: Infer pattern
// XXX: Check pattern
// XXX: Bind pattern

pub(crate) fn check_and_bind(ctx: &mut Context, env: &mut Environment, p: Pattern, v: Value, mut v_type: Value) -> Result<(), ElaborationError> {
    match *p.0 {
        Value::BlankBinder => {
            let t = quote(ctx, env, v)?;
            check(ctx, env, t, v_type)?;

            Ok(())
        },
        Value::Binder(x) => { 
            let t = quote(ctx, env, v.clone())?;
            check(ctx, env, t, v_type.clone())?;

            ctx.insert_type(x, v_type);
            env.insert(x, v); 
            Ok(())
        },
        Value::Annotation(p_inner, annotated_type) => {
            let annotated_type = check(ctx, env, *annotated_type, Value::Type)?;
            let mut annotated_type = evaluate(ctx, env, annotated_type)?;
            if equal(ctx, env, &mut v_type, &mut annotated_type) {
                check_and_bind(ctx, env, *p_inner, v, annotated_type)
            }
            else {
                Err(ElaborationError::ExpectedTypeFoundType(*p_inner, annotated_type, v_type))
            }
        },
        Value::Type |
        Value::FuncType(..) |
        Value::TupleType(..) |
        Value::Unit |
        Value::Nat |
        Value::Str |
        Value::Func(_, _, _) |
        Value::ExternalFunc(_) |
        Value::Tuple(_, _) |
        Value::EmptyTuple |
        Value::NatNum(_) |
        Value::StrLiteral(_) |
        Value::Neutral(_) |
        Value::Hole(_) => {
            todo!()
        }
    }
    // match p {
    //     Pattern::Ignore => Ok(()),
    //     Pattern::EmptyTuple => if def_equal(ctx, env, &mut v_type, &mut Value::Unit) {
    //         Ok(())
    //     } else {
    //         Err(ElaborationError::ExpectedTypeFoundType(Pattern::EmptyTuple, Value::Unit, v_type))
    //     },
    //     Pattern::Annotation(p_inner, asserted_type) => {
    //         let asserted_type = check(ctx, env, *asserted_type, Value::Type)?;
    //         let mut asserted_type = evaluate(ctx, env, asserted_type)?;
    //         if def_equal(ctx, env, &mut v_type, &mut asserted_type) {
    //             check_and_bind(ctx, env, *p_inner, v, asserted_type)
    //         }
    //         else {
    //             Err(ElaborationError::ExpectedTypeFoundType(*p_inner, asserted_type, v_type))
    //         }
    //     },
    //     Pattern::Generic(x) => Ok({ 
    //         ctx.insert_type(x, v_type);
    //         env.insert(x, v); 
    //     }),
    // }
}

// TODO: remove bind in favour of check_and_bind with holes
pub(crate) fn bind(ctx: &mut Context, env: &mut Environment, p: &Pattern, mut v: Value) -> Result<(), ElaborationError> {
    // match p {
    //     Pattern::Ignore => Ok(()),
    //     Pattern::EmptyTuple => Ok(()),
    //     Pattern::Annotation(p_inner, _) => bind(ctx, env, p_inner, v),
    //     Pattern::Generic(x) => Ok({
    //         env.insert(*x, v); 
    //     })
    // }

    unify(ctx, env, &mut *p.0.clone(), &mut v)?;
    Ok(())
}

pub(crate) fn infer_pattern(ctx: &mut Context, env: &mut Environment, p: Pattern) -> Result<Value, ElaborationError> {
    let (ty, has_holes) = infer_holed_pattern(ctx, env, p)?;
    if has_holes {
        panic!("oi! this pattern shouldnt' 'ave 'oles innit") // TODO: write error messages that aren't dumb/use ElaborationError
    }
    Ok(ty)
}

pub(crate) fn infer_holed_pattern(ctx: &mut Context, env: &mut Environment, p: Pattern) -> Result<(Value, bool), ElaborationError> {
    match *p.0 {
        Value::Tuple(_, _) => todo!(),

        Value::EmptyTuple => Ok((Value::Unit, false)),
        Value::NatNum(_) => Ok((Value::Nat, false)),
        Value::StrLiteral(_) => Ok((Value::Str, false)),

        Value::BlankBinder | Value::Binder(_) => {
            let h = env.new_hole();
            
            Ok((Value::Hole(h), true))
        },
        Value::Annotation(p_inner, type_term) => {
            let mut ty = evaluate(ctx, env, *type_term.clone())?;
            check_holed_pattern(ctx, env, *p_inner, &mut ty)?;
            Ok((ty, false))
        },
        
        Value::Str |
        Value::Nat |
        Value::Unit |
        Value::Neutral(_) |
        Value::Hole(_) |
        Value::Type |
        Value::FuncType(..) |
        Value::TupleType(..) |
        Value::Func(..) |
        Value::ExternalFunc(_) => unreachable!(),
    }
}

// Only ever used in `infer_holed_pattern`, so `ty` is assumed to be fully evaluated
fn check_holed_pattern(ctx: &mut Context, env: &mut Environment, p: Pattern, ty: &mut Value) -> Result<(), ElaborationError> {
    match (*p.0, ty) {
        (Value::Tuple(_, _), ty) => todo!(),

        (Value::Annotation(p_inner, type_term), ty) => todo!(),

        (Value::EmptyTuple, Value::Unit) | 
        (Value::NatNum(_), Value::Nat) |
        (Value::StrLiteral(_), Value::Str) |
        (Value::BlankBinder | Value::Binder(_), _) => Ok(()),

        (p @ (Value::EmptyTuple | Value::NatNum(_) | Value::StrLiteral(_)), ty) => todo!(), // TODO: Expected X, Found Y error but for incongruent annotations

        (Value::Str |
        Value::Nat |
        Value::Unit |
        Value::Neutral(_) |
        Value::Hole(_) |
        Value::Type |
        Value::FuncType(..) |
        Value::TupleType(..) |
        Value::Func(..) |
        Value::ExternalFunc(_), _) => unreachable!(),
    }
}

// fn _infer_pattern(ctx: &mut Context, env: &mut Environment, p: &Pattern) -> Result<(Pattern, Value), ElaborationError> {
//     match p.0 {
//         p @ Value::BlankBinder => Ok((p.into(), env.new_hole().into())),
//         _ => todo!()
//     }


//     // match p {
//     //     Pattern::Ignore => panic!("Can't infer type of _"), // TODO: replace with proper error
//     //     Pattern::EmptyTuple => Ok((Pattern::EmptyTuple, Value::Unit)),
//     //     Pattern::Generic(x) => panic!("Can't infer type of {:?}", x), // TODO: replace with proper error
//     //     Pattern::Annotation(p_inner, p_type) => {
//     //         let p_type = check(ctx, env, *p_type, Value::Type)?;
//     //         let p_type = evaluate(ctx, env, p_type)?;
//     //         check_pattern(ctx, env, *p_inner.clone(), p_type.clone())?;
//     //         Ok((*p_inner, p_type))
//     //     },
//     // }
    
// }