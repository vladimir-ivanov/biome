/* should not generate diagnostics */
config.newIsCap = config.newIsCap !== false;
var a = x === 2 ? 'Yes' : 'No';
var a = x === 2 ? true : 'No';
var a = x === 2 ? 'Yes' : false;
var a = x === 2 ? 'true' : 'false';
var a = foo ? foo : bar;
var value = 'a';var canSet = true;var result = value || (canSet ? 'unset' : 'can not set');
var a = foo ? bar : foo;
foo ? bar : foo;
var a = f(x ? x : 1);
f(x ? x : 1);
foo ? foo : bar;
var a = foo ? 'Yes' : foo;
