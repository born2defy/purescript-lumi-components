(window.webpackJsonp=window.webpackJsonp||[]).push([[93],{"0URS":function(n,t,u){"use strict";var r=u("4Bm9"),i=u("sAO9"),e=u("GLdZ"),c=u("Xnug"),o=function(n,t,u){this.Functor0=n,this.collect=t,this.distribute=u},f=new o(function(){return e.functorIdentity},function(n){return function(t){return function(u){return e.Identity(i.map(n)(function(n){return c.unwrap(e.newtypeIdentity)(t(n))})(u))}}},function(n){return function(t){return e.Identity(i.map(n)(c.unwrap(e.newtypeIdentity))(t))}}),s=function(n){return n.distribute},d=new o(function(){return i.functorFn},function(n){return function(t){return function(u){return s(d)(n)(i.map(n)(t)(u))}}},function(n){return function(t){return function(u){return i.map(n)(function(n){return n(u)})(t)}}}),p=function(n){return n.collect};n.exports={collect:p,distribute:s,Distributive:o,distributeDefault:function(n){return function(t){return p(n)(t)(r.identity(r.categoryFn))}},collectDefault:function(n){return function(t){return function(u){return function(r){return s(n)(t)(i.map(t)(u)(r))}}}},cotraverse:function(n){return function(t){return function(u){return function(r){return i.map(n.Functor0())(u)(s(n)(t)(r))}}}},distributiveIdentity:f,distributiveFunction:d};u("sygH")}}]);