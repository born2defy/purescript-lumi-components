(window.webpackJsonp=window.webpackJsonp||[]).push([[5],{VRBs:function(e,r,n){"use strict";var t=n("4Bm9"),u=n("CgSG"),o=n("t5nC"),f=n("rkps"),i=n("6YwU"),s=n("AKjc"),a=n("j9/M"),l=n("wIFv"),c=n("/hSk"),m=function(e){this.formDefaultsRecordBuilder=e},w=function(e){this.formDefaults=e},d=new w(s.unit),D=new w(""),R=new m(function(e){return t.identity(l.categoryBuilder)}),p=function(e){return e.formDefaultsRecordBuilder},B=new w(0),y=new w(f.Nothing.value),h=new w(!1),v=new w([]),g=function(e){return e.formDefaults};e.exports={FormDefaults:w,formDefaults:g,FormDefaultsRecord:m,formDefaultsRecordBuilder:p,formDefaultsUnit:d,formDefaultsBoolean:h,formDefaultsNumber:B,formDefaultsString:D,formDefaultsArray:v,formDefaultsMaybe:y,formDefaultsEither:function(e){return new w(new o.Left(g(e)))},formDefaultsValidated:function(e){return new w(new a.Fresh(g(e)))},formDefaultsRecord:function(e){return function(e){return new w(l.build(p(e)(c.RLProxy.value))({}))}},formDefaultsRecordNil:R,formDefaultsRecordCons:function(e){return function(r){return function(n){return function(t){return function(o){return new m(function(f){var s=p(n)(c.RLProxy.value),a=l.insert(o)(t)(e)(i.SProxy.value)(g(r));return u.compose(l.semigroupoidBuilder)(a)(s)})}}}}}};n("sygH"),n("E2BJ")}}]);