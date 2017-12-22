var result = (function(){
var state = {};
function assertNum(x){
    if (typeof(x) !== 'number') throw 'not a number'
}
function assertBool(x){
    if (typeof(x) !== 'boolean') throw 'not a boolean'
}
function add(xfunc, yfunc){
    let x = callIfFunc(xfunc);
    let y = callIfFunc(yfunc);
    assertNum(x);
    assertNum(y);
    return x+y;
}
function mul(xfunc, yfunc){
    let x = callIfFunc(xfunc);
    let y = callIfFunc(yfunc);
    assertNum(x);
    assertNum(y);
    return x*y;
}
function and(xfunc, yfunc){
    let x = callIfFunc(xfunc);
    let y = callIfFunc(yfunc);
    assertBool(x);
    assertBool(y);
    return x && y;
}
function or(xfunc, yfunc){
    let x = callIfFunc(xfunc);
    let y = callIfFunc(yfunc);
    assertBool(x);
    assertBool(y);
    return x || y;
}
function eq(xfunc, yfunc){
    let x = callIfFunc(xfunc);
    let y = callIfFunc(yfunc);
    if (['number', 'boolean', 'string'].indexOf(typeof(x)) >= 0 &&['number', 'boolean', 'string'].indexOf(typeof(y)) >= 0) {
      if (typeof(x) != typeof(y)){
          return false;
      }
      return x === y;
    }
    throw 'invalid type';
}
function neq(xfunc, yfunc){
    return !eq(xfunc, yfunc);
}
function _if(condfunc, iftfunc, ifffunc){
    let cond = callIfFunc(condfunc);
    let ift = callIfFunc(iftfunc);
    let iff = callIfFunc(ifffunc);
    assertBool(cond);
    return cond ? ift : iff;
}
function set(variableFunc, valFunc){
    let variable = callIfFunc(variableFunc);
    let val = callIfFunc(valFunc);
    state[variable] = val;
    return true;
}
function get(variableFunc){
    let variable = callIfFunc(variableFunc);
    if (typeof(state[variable]) === 'undefined'){
        throw 'variable not defined';
    }
    return state[variable];
}
function _do(){
    var args = [].slice.call(arguments);
    var result = null;
    for (let i=0; i<args.length; i++) {
        result = args[i]();
    }
    return result;
}
function _while(condFunc, exprFunc){
    while (callIfFunc(condFunc)){
        callIfFunc(exprFunc);
    }
}
function print(variableFunc){
    let variable = callIfFunc(variableFunc);
    console.info(variable);
}
function callIfFunc(obj){
    return (typeof(obj) === 'function') ? obj() : obj;
}
var result={placeholder};
return result();
})()