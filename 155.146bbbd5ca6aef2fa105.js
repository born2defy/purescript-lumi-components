(window.webpackJsonp=window.webpackJsonp||[]).push([[155],{"1liq":function(e,t,n){"use strict";var r,o,i=n("UCB5"),a=n("v0Xu"),u=n("Z7LG"),l=n("4Bm9"),c=n("t5nC"),s=n("zo+L"),f=n("YrJM"),d=n("sAO9"),m=n("ZpT1"),p=n("rkps"),y=n("T4xb"),h=n("NucT"),w=n("UhtL"),S=n("Whft"),v=n("6YwU"),b=n("AKjc"),g=n("jcJx"),I=n("yb+h"),D=n("GDhj"),x=n("+xh4"),F=n("Woux"),R=n("mjQF"),z=n("E2BJ"),C=n("VRBs"),N=n("ieUD"),W=n("j9/M"),L=n("mLKm"),E=n("Fs2P"),J=n("z1Dq"),A=n("Fwyz"),B=n("Q6fk"),M=n("qajM"),V=n("FFyt"),k=n("HB2f"),q=n("coXC"),T=function(){function e(){}return e.value=new e,e}(),H=function(){function e(){}return e.value=new e,e}(),P=function(){function e(){}return e.value=new e,e}(),j=function(){function e(e){this.value0=e}return e.create=function(t){return new e(t)},e}(),U=function(){function e(){}return e.value=new e,e}(),_=function(){function e(e){this.value0=e}return e.create=function(t){return new e(t)},e}(),O=function(){function e(e){this.value0=e}return e.create=function(t){return new e(t)},e}(),X=function(e){return function(t){return N.parallel("thirdStepForm")(u.discard(u.discardUnit)(N.bindSeqFormBuilder)(N.sequential("text")(z.static(A.p_("Alright! What I know so far is that your name is "+e.firstName+" "+e.lastName+p.maybe("")(function(e){return", you are "+w.show(w.showInt)(e)+" years old"})(e.age)+", live in "+t.country+" and "+(t.workFromHome?"do":"do not")+" work from home. Also, "+p.maybe("")(function(e){return" you are "+w.show(w.showNumber)(e)+" inches tall, and "})(t.height)+" your favorite color is "+t.favoriteColor+"."))))(function(){return u.bind(N.bindSeqFormBuilder)(N.sequential("hasAdditionalInfo")(z.indent("Do you want to give me additional information?")(E.Neither.value)(z.focus(function(e){return m.prop(new v.IsSymbol(function(){return"hasAdditionalInfo"}))()()(v.SProxy.value)(e)})(z.switch))))(function(e){return u.bind(N.bindSeqFormBuilder)(N.sequential("additionalInfo")(e?z.indent("Additional information")(E.Required.value)(z.focus(function(e){return m.prop(new v.IsSymbol(function(){return"additionalInfo"}))()()(v.SProxy.value)(e)})(d.map(N.functorFormBuilder)(function(e){return p.Just.create(S.toString(e))})(W.validated(W.canValidateAny)(W.nonEmpty("Additional info"))(z.textarea)))):i.pure(N.applicativeFormBuilder)(p.Nothing.value)))(function(t){return i.pure(N.applicativeSeqFormBuilder)({hasAdditionalInfo:e,additionalInfo:t})})})}))}},Y=function(e){return a.apply(N.applyFormBuilder)(a.apply(N.applyFormBuilder)(a.apply(N.applyFormBuilder)(d.map(N.functorFormBuilder)(function(e){return function(t){return function(n){return function(r){return{country:S.toString(e),workFromHome:t,height:n,favoriteColor:r}}}}})(z.indent("Country")(E.Required.value)(z.focus(function(e){return m.prop(new v.IsSymbol(function(){return"country"}))()()(v.SProxy.value)(e)})(W.validated(W.canValidateAny)(W.nonEmpty("Country"))(z.textbox)))))(p.isJust(e)&&h.lessThan(p.ordMaybe(h.ordInt))(e)(new p.Just(15))?i.pure(N.applicativeFormBuilder)(!1):z.indent("Work from home?")(E.Neither.value)(z.focus(function(e){return m.prop(new v.IsSymbol(function(){return"workFromHome"}))()()(v.SProxy.value)(e)})(z.switch))))(z.indent("Height (in)")(E.Optional.value)(z.focus(function(e){return m.prop(new v.IsSymbol(function(){return"height"}))()()(v.SProxy.value)(e)})(W.validated(W.canValidateAny)(W.optional(W.validNumber("Height")))(z.number({min:new p.Just(0),max:p.Nothing.value,step:L.Any.value}))))))(z.indent("Favorite color")(E.Required.value)(z.focus(function(e){return m.prop(new v.IsSymbol(function(){return"favoriteColor"}))()()(v.SProxy.value)(e)})(W.validated(W.canValidateAny)(W.nonNull("Favorite color"))(z.select(l.identity(l.categoryFn))(i.pure(p.applicativeMaybe))([{label:"Red",value:"red"},{label:"Green",value:"green"},{label:"Blue",value:"blue"}])))))},G=a.apply(N.applyFormBuilder)(a.apply(N.applyFormBuilder)(d.map(N.functorFormBuilder)(function(e){return function(t){return function(n){return{firstName:S.toString(e),lastName:S.toString(t),age:n}}}})(z.indent("First name")(E.Required.value)(z.focus(function(e){return m.prop(new v.IsSymbol(function(){return"firstName"}))()()(v.SProxy.value)(e)})(W.validated(W.canValidateAny)(W.nonEmpty("First name"))(z.textbox)))))(z.indent("Last name")(E.Required.value)(z.focus(function(e){return m.prop(new v.IsSymbol(function(){return"lastName"}))()()(v.SProxy.value)(e)})(W.validated(W.canValidateAny)(W.nonEmpty("Last name"))(z.textbox)))))(z.indent("Age")(E.Neither.value)(z.focus(function(e){return m.prop(new v.IsSymbol(function(){return"age"}))()()(v.SProxy.value)(e)})(W.validated(W.canValidateAny)(W.optional(W.validInt("Age")))(z.number({min:new p.Just(0),max:p.Nothing.value,step:new L.Step(1)}))))),Q=u.bind(B.bindWizard)(B.step(T.value)(z.focus(function(e){return m.prop(new v.IsSymbol(function(){return"firstStep"}))()()(v.SProxy.value)(e)})(G)))(function(e){return u.bind(B.bindWizard)(B.step(H.value)(z.focus(function(e){return m.prop(new v.IsSymbol(function(){return"secondStep"}))()()(v.SProxy.value)(e)})(Y(e.age))))(function(t){return u.bind(B.bindWizard)(B.step(P.value)(z.focus(function(e){return m.prop(new v.IsSymbol(function(){return"thirdStep"}))()()(v.SProxy.value)(e)})(X(e)(t))))(function(n){return i.pure(B.applicativeWizard)({text:"I know now that your name is "+e.firstName+" "+e.lastName+p.maybe("")(function(e){return", you are "+w.show(w.showInt)(e)+" years old"})(e.age)+", live in "+t.country+" and "+(t.workFromHome?"do":"do not")+" work from home. Also, "+p.maybe("")(function(e){return" you are "+w.show(w.showNumber)(e)+" inches tall, and "})(t.height)+" your favorite color is "+t.favoriteColor+".",occupation:"PureScript programmer"})})})}),Z=new s.Eq(function(e){return function(t){return e instanceof T&&t instanceof T||(e instanceof H&&t instanceof H||e instanceof P&&t instanceof P)}}),K=(r=function(e){return function(t){if(t instanceof j)return e.setState(function(n){return{formData:t.value0(e.state.formData),wizardStep:n.wizardStep,result:n.result}});if(t instanceof U){var n=B.stepIdentifier(e.state.wizardStep);if(n instanceof p.Just&&n.value0 instanceof T)return e.setState(function(e){return{formData:{firstStep:W.setModified()(D.hmapRecord()(D.mapRecordWithIndexCons(new v.IsSymbol(function(){return"age"}))(D.constMapping(W.modifyValidated))(D.mapRecordWithIndexCons(new v.IsSymbol(function(){return"firstName"}))(D.constMapping(W.modifyValidated))(D.mapRecordWithIndexCons(new v.IsSymbol(function(){return"lastName"}))(D.constMapping(W.modifyValidated))(D.mapRecordWithIndexNil)()())()())()()))(e.formData.firstStep),secondStep:e.formData.secondStep,thirdStep:e.formData.thirdStep},wizardStep:e.wizardStep,result:e.result}});if(n instanceof p.Just&&n.value0 instanceof H)return e.setState(function(e){return{formData:{firstStep:e.formData.firstStep,secondStep:W.setModified()(D.hmapRecord()(D.mapRecordWithIndexCons(new v.IsSymbol(function(){return"country"}))(D.constMapping(W.modifyValidated))(D.mapRecordWithIndexCons(new v.IsSymbol(function(){return"favoriteColor"}))(D.constMapping(W.modifyValidated))(D.mapRecordWithIndexCons(new v.IsSymbol(function(){return"height"}))(D.constMapping(W.modifyValidated))(D.mapRecordWithIndexCons(new v.IsSymbol(function(){return"workFromHome"}))(D.constMapping(W.modifyValidatedA))(D.mapRecordWithIndexNil)()())()())()())()()))(e.formData.secondStep),thirdStep:e.formData.thirdStep},wizardStep:e.wizardStep,result:e.result}});if(n instanceof p.Just&&n.value0 instanceof P)return e.setState(function(e){return{formData:{firstStep:e.formData.firstStep,secondStep:e.formData.secondStep,thirdStep:W.setModified()(D.hmapRecord()(D.mapRecordWithIndexCons(new v.IsSymbol(function(){return"additionalInfo"}))(D.constMapping(W.modifyValidated))(D.mapRecordWithIndexCons(new v.IsSymbol(function(){return"hasAdditionalInfo"}))(D.constMapping(W.modifyValidatedA))(D.mapRecordWithIndexNil)()())()()))(e.formData.thirdStep)},wizardStep:e.wizardStep,result:e.result}});if(n instanceof p.Nothing)return i.pure(g.applicativeEffect)(b.unit);throw new Error("Failed pattern match at Lumi.Components.Examples.Wizard (line 48, column 11 - line 52, column 33): "+[n.constructor.name])}if(t instanceof _)return e.setState(function(e){return{formData:e.formData,wizardStep:t.value0,result:e.result}});if(t instanceof O)return e.setState(function(e){return{formData:e.formData,wizardStep:e.wizardStep,result:new p.Just(t.value0)}});throw new Error("Failed pattern match at Lumi.Components.Examples.Wizard (line 43, column 7 - line 58, column 51): "+[t.constructor.name])}},o={formData:C.formDefaults(C.formDefaultsRecord()(C.formDefaultsRecordCons(new v.IsSymbol(function(){return"firstStep"}))(C.formDefaultsRecord()(C.formDefaultsRecordCons(new v.IsSymbol(function(){return"age"}))(C.formDefaultsValidated(C.formDefaultsString))(C.formDefaultsRecordCons(new v.IsSymbol(function(){return"firstName"}))(C.formDefaultsValidated(C.formDefaultsString))(C.formDefaultsRecordCons(new v.IsSymbol(function(){return"lastName"}))(C.formDefaultsValidated(C.formDefaultsString))(C.formDefaultsRecordNil)()())()())()()))(C.formDefaultsRecordCons(new v.IsSymbol(function(){return"secondStep"}))(C.formDefaultsRecord()(C.formDefaultsRecordCons(new v.IsSymbol(function(){return"country"}))(C.formDefaultsValidated(C.formDefaultsString))(C.formDefaultsRecordCons(new v.IsSymbol(function(){return"favoriteColor"}))(C.formDefaultsValidated(C.formDefaultsMaybe))(C.formDefaultsRecordCons(new v.IsSymbol(function(){return"height"}))(C.formDefaultsValidated(C.formDefaultsString))(C.formDefaultsRecordCons(new v.IsSymbol(function(){return"workFromHome"}))(C.formDefaultsBoolean)(C.formDefaultsRecordNil)()())()())()())()()))(C.formDefaultsRecordCons(new v.IsSymbol(function(){return"thirdStep"}))(C.formDefaultsRecord()(C.formDefaultsRecordCons(new v.IsSymbol(function(){return"additionalInfo"}))(C.formDefaultsValidated(C.formDefaultsString))(C.formDefaultsRecordCons(new v.IsSymbol(function(){return"hasAdditionalInfo"}))(C.formDefaultsBoolean)(C.formDefaultsRecordNil)()())()()))(C.formDefaultsRecordNil)()())()())()())),result:p.Nothing.value,wizardStep:B.liftStep(Q)},M.make()(M.createComponent("WizardExample"))({initialState:o,render:function(e){return F.column_([R.example(F.column({style:V.css({alignSelf:"stretch"}),children:function(){if(e.state.result instanceof p.Nothing)return[B.wizard({step:e.state.wizardStep,value:e.state.formData,onChange:function(t){return r(e)(j.create(t))},forceTopLabels:!1,inlineTable:!1,readonly:!1}),J.row({style:V.css({justifyContent:"flex-end"}),children:(t=B.previousStep(e.state.wizardStep),n=B.resumeStep(e.state.wizardStep)({readonly:!1})(e.state.formData),o=p.maybe(!1)(c.isRight)(n),[x.button({accessibilityLabel:x.secondary.accessibilityLabel,color:x.secondary.color,disabled:p.isNothing(t),onPress:p.maybe(I.mkEffectFn1(y.mempty(y.monoidFn(g.monoidEffect(y.monoidUnit)))))(function(t){return k.capture(l.identity(q.categoryBuilder))(f.const(r(e)(_.create(t))))})(t),size:x.secondary.size,style:x.secondary.style,testId:x.secondary.testId,title:"Back",type:x.secondary.type,loading:x.secondary.loading}),x.button({accessibilityLabel:x.primary.accessibilityLabel,color:x.primary.color,disabled:x.primary.disabled,onPress:k.capture_(function(){if(n instanceof p.Nothing)return r(e)(U.value);if(n instanceof p.Just&&n.value0 instanceof c.Left)return r(e)(new _(n.value0.value0));if(n instanceof p.Just&&n.value0 instanceof c.Right)return r(e)(new O(n.value0.value0));throw new Error("Failed pattern match at Lumi.Components.Examples.Wizard (line 97, column 39 - line 100, column 81): "+[n.constructor.name])}()),size:x.primary.size,style:V.css({marginLeft:"12px"}),testId:x.primary.testId,title:o?"Submit":"Next",type:x.primary.type,loading:x.primary.loading})])})];var t,n,o;if(e.state.result instanceof p.Just)return[A.p_("Dilly dily data, my magic's a done! I'm a Wizard, after all."),A.p_(e.state.result.value0.text),A.p_("From all this information, I can only conclude that yourself are a:"),A.text({children:[V.text("PureScript programmer!")],className:A.sectionHeader.className,color:A.sectionHeader.color,style:V.css({textAlign:"center"}),tag:A.sectionHeader.tag,testId:A.sectionHeader.testId})];throw new Error("Failed pattern match at Lumi.Components.Examples.Wizard (line 65, column 17 - line 113, column 22): "+[e.state.result.constructor.name])}()}))])}})(b.unit));e.exports={OnChange:j,SetModified:U,SetWizardStep:_,Finalize:O,docs:K,FirstStep:T,SecondStep:H,ThirdStep:P,exampleWizard:Q,firstStepForm:G,secondStepForm:Y,thirdStepForm:X,eqStep:Z};n("sygH"),n("RgHw")},Q6fk:function(e,t,n){"use strict";var r=n("eARU"),o=n("J9YH"),i=n("t7hT"),a=n("t5nC"),u=n("sAO9"),l=n("rkps"),c=n("T4xb"),s=n("Xnug"),f=n("NucT"),d=n("E2BJ"),m=n("ieUD"),p=n("qajM"),y=n("dZMS"),h=function(e){return e},w=function(e){return e},S=new s.Newtype(function(e){return e},w),v=new s.Newtype(function(e){return e},h),b=r.freeMonad,g=r.freeFunctor,I=new u.Functor(function(e){return function(t){return{run:u.map(u.functorFn)(u.map(u.functorFn)(function(t){return{result:u.map(l.functorMaybe)(e)(t.result),form:t.form}}))(t.run),step:t.step}}}),D=function(e){return w((t=i.lmap(a.bifunctorEither)(u.map(I)(h))(r.resume(I)(s.un(v)(h)(e))),{previous:l.Nothing.value,current:t}));var t},x=r.freeBind,F=r.freeApply,R=r.freeApplicative;e.exports={step:function(e){return function(t){var n=d.buildConcurrently(t);return h(r.liftF({step:e,run:function(e){return function(r){return{form:function(t){return function(o){return n(y.unsafeUnion(e)({value:r,onChange:function(e){return o},forceTopLabels:t.forceTopLabels,inlineTable:t.inlineTable}))}},result:m.revalidate(t)(e)(r)}}}}))}},revalidate:function(e){return function(t){return function(n){return r.foldFree(o.monadRecMaybe)(function(e){return e.run(t)(n).result})(s.un(v)(h)(e))}}},stepIdentifier:function(e){if(e.current instanceof a.Left)return new l.Just(e.current.value0.step);if(e.current instanceof a.Right)return l.Nothing.value;throw new Error("Failed pattern match at Lumi.Components.Wizard (line 105, column 3 - line 107, column 23): "+[e.current.constructor.name])},liftStep:D,resumeStep:function(e){return function(t){return function(n){if(e.current instanceof a.Right)return new l.Just(new a.Right(e.current.value0));if(e.current instanceof a.Left){var o=e.current.value0.run(t)(n);if(o.result instanceof l.Nothing)return l.Nothing.value;if(o.result instanceof l.Just){var c=r.resume(I)(s.un(v)(h)(o.result.value0));if(c instanceof a.Right)return new l.Just(new a.Right(c.value0));if(c instanceof a.Left)return l.Just.create(a.Left.create({previous:new l.Just(e),current:i.lmap(a.bifunctorEither)(u.map(I)(h))(r.resume(I)(s.un(v)(h)(o.result.value0)))}));throw new Error("Failed pattern match at Lumi.Components.Wizard (line 131, column 13 - line 137, column 20): "+[c.constructor.name])}throw new Error("Failed pattern match at Lumi.Components.Wizard (line 128, column 9 - line 137, column 20): "+[o.result.constructor.name])}throw new Error("Failed pattern match at Lumi.Components.Wizard (line 122, column 3 - line 137, column 20): "+[e.current.constructor.name])}}},previousStep:function(e){return s.un(S)(w)(e).previous},gotoStep:function(e){return function(t){return function(n){return function(o){return function(c){return function(n){var d,m=!1;function p(d){if(d.current instanceof a.Right)return m=!0,l.Nothing.value;if(d.current instanceof a.Left){if(f.greaterThanOrEq(e)(d.current.value0.step)(t))return m=!0,new l.Just(d);var p=d.current.value0.run(o)(c);if(p.result instanceof l.Nothing)return m=!0,l.Nothing.value;if(p.result instanceof l.Just){var y=r.resume(I)(s.un(v)(h)(p.result.value0));if(y instanceof a.Right)return m=!0,l.Nothing.value;if(y instanceof a.Left)return void(n={previous:new l.Just(d),current:i.lmap(a.bifunctorEither)(u.map(I)(h))(r.resume(I)(s.un(v)(h)(p.result.value0)))});throw new Error("Failed pattern match at Lumi.Components.Wizard (line 173, column 21 - line 179, column 28): "+[y.constructor.name])}throw new Error("Failed pattern match at Lumi.Components.Wizard (line 170, column 17 - line 179, column 28): "+[p.result.constructor.name])}throw new Error("Failed pattern match at Lumi.Components.Wizard (line 161, column 7 - line 179, column 28): "+[d.current.constructor.name])}for(;!m;)d=p(n);return d}(D(n))}}}}},wizard:function(e){var t=s.un(S)(w)(e.step).current;if(t instanceof a.Right)return c.mempty(p.monoidJSX);if(t instanceof a.Left)return t.value0.run(e)(e.value).form({forceTopLabels:e.forceTopLabels,inlineTable:e.inlineTable})(e.onChange);throw new Error("Failed pattern match at Lumi.Components.Wizard (line 193, column 3 - line 200, column 54): "+[t.constructor.name])},functorWizard:g,applyWizard:F,applicativeWizard:R,bindWizard:x,monadWizard:b};n("sygH"),n("jcJx"),n("3kYz")}}]);