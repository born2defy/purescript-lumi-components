(window.webpackJsonp=window.webpackJsonp||[]).push([[109],{Gs4Q:function(e){e.exports=JSON.parse('{"efVersion":"0.13.5","efModuleName":["Lumi","Components","Examples","Upload"],"efExports":[{"ValueRef":[{"start":[1,1],"name":"docs/Examples/Upload.example.purs","end":[195,40]},{"Ident":"component"}]},{"TypeRef":[{"start":[1,1],"name":"docs/Examples/Upload.example.purs","end":[195,40]},"Action",["NoOp","SetReadonly","ImageEx","FileEx","AvatarEx","StartUpload","InitializeUploadBuffer"]]},{"ValueRef":[{"start":[1,1],"name":"docs/Examples/Upload.example.purs","end":[195,40]},{"Ident":"docs"}]},{"ValueRef":[{"start":[1,1],"name":"docs/Examples/Upload.example.purs","end":[195,40]},{"Ident":"debug"}]}],"efImports":[{"eiModule":["Prim"],"eiImportType":{"Implicit":[]},"eiImportedAs":["Prim"]},{"eiModule":["Prim"],"eiImportType":{"Implicit":[]},"eiImportedAs":null},{"eiModule":["Prelude"],"eiImportType":{"Implicit":[]},"eiImportedAs":null},{"eiModule":["Control","Coroutine","Aff"],"eiImportType":{"Explicit":[{"ValueRef":[{"start":[5,31],"name":"docs/Examples/Upload.example.purs","end":[5,36]},{"Ident":"close"}]},{"ValueRef":[{"start":[5,38],"name":"docs/Examples/Upload.example.purs","end":[5,42]},{"Ident":"emit"}]},{"ValueRef":[{"start":[5,44],"name":"docs/Examples/Upload.example.purs","end":[5,54]},{"Ident":"produceAff"}]}]},"eiImportedAs":null},{"eiModule":["Data","Array"],"eiImportType":{"Explicit":[{"ValueRef":[{"start":[6,20],"name":"docs/Examples/Upload.example.purs","end":[6,24]},{"Ident":"head"}]}]},"eiImportedAs":null},{"eiModule":["Data","Foldable"],"eiImportType":{"Explicit":[{"ValueRef":[{"start":[7,23],"name":"docs/Examples/Upload.example.purs","end":[7,30]},{"Ident":"foldMap"}]}]},"eiImportedAs":null},{"eiModule":["Data","Int"],"eiImportType":{"Implicit":[]},"eiImportedAs":["Int"]},{"eiModule":["Data","Maybe"],"eiImportType":{"Explicit":[{"TypeRef":[{"start":[9,20],"name":"docs/Examples/Upload.example.purs","end":[9,29]},"Maybe",null]},{"ValueRef":[{"start":[9,31],"name":"docs/Examples/Upload.example.purs","end":[9,40]},{"Ident":"fromMaybe"}]},{"ValueRef":[{"start":[9,42],"name":"docs/Examples/Upload.example.purs","end":[9,47]},{"Ident":"maybe"}]}]},"eiImportedAs":null},{"eiModule":["Effect"],"eiImportType":{"Explicit":[{"TypeRef":[{"start":[10,16],"name":"docs/Examples/Upload.example.purs","end":[10,22]},"Effect",[]]}]},"eiImportedAs":null},{"eiModule":["Effect","Aff"],"eiImportType":{"Explicit":[{"TypeRef":[{"start":[11,20],"name":"docs/Examples/Upload.example.purs","end":[11,36]},"Milliseconds",null]},{"ValueRef":[{"start":[11,38],"name":"docs/Examples/Upload.example.purs","end":[11,43]},{"Ident":"delay"}]},{"ValueRef":[{"start":[11,45],"name":"docs/Examples/Upload.example.purs","end":[11,55]},{"Ident":"launchAff_"}]}]},"eiImportedAs":null},{"eiModule":["Effect","Aff","AVar"],"eiImportType":{"Explicit":[{"TypeRef":[{"start":[12,25],"name":"docs/Examples/Upload.example.purs","end":[12,29]},"AVar",[]]}]},"eiImportedAs":null},{"eiModule":["Effect","Aff","AVar"],"eiImportType":{"Implicit":[]},"eiImportedAs":["AVar"]},{"eiModule":["Effect","Class"],"eiImportType":{"Explicit":[{"ValueRef":[{"start":[14,22],"name":"docs/Examples/Upload.example.purs","end":[14,32]},{"Ident":"liftEffect"}]}]},"eiImportedAs":null},{"eiModule":["Effect","Console"],"eiImportType":{"Explicit":[{"ValueRef":[{"start":[15,24],"name":"docs/Examples/Upload.example.purs","end":[15,27]},{"Ident":"log"}]}]},"eiImportedAs":null},{"eiModule":["Effect","Random"],"eiImportType":{"Explicit":[{"ValueRef":[{"start":[16,23],"name":"docs/Examples/Upload.example.purs","end":[16,34]},{"Ident":"randomRange"}]}]},"eiImportedAs":null},{"eiModule":["Effect","Uncurried"],"eiImportType":{"Explicit":[{"ValueRef":[{"start":[17,26],"name":"docs/Examples/Upload.example.purs","end":[17,38]},{"Ident":"runEffectFn2"}]}]},"eiImportedAs":null},{"eiModule":["Lumi","Components","Button"],"eiImportType":{"Explicit":[{"ValueRef":[{"start":[18,32],"name":"docs/Examples/Upload.example.purs","end":[18,38]},{"Ident":"button"}]},{"ValueRef":[{"start":[18,40],"name":"docs/Examples/Upload.example.purs","end":[18,47]},{"Ident":"primary"}]}]},"eiImportedAs":["Button"]},{"eiModule":["Lumi","Components","Column"],"eiImportType":{"Explicit":[{"ValueRef":[{"start":[19,32],"name":"docs/Examples/Upload.example.purs","end":[19,38]},{"Ident":"column"}]},{"ValueRef":[{"start":[19,40],"name":"docs/Examples/Upload.example.purs","end":[19,47]},{"Ident":"column_"}]}]},"eiImportedAs":null},{"eiModule":["Lumi","Components","Input"],"eiImportType":{"Implicit":[]},"eiImportedAs":["Input"]},{"eiModule":["Lumi","Components","LabeledField"],"eiImportType":{"Explicit":[{"ValueRef":[{"start":[21,38],"name":"docs/Examples/Upload.example.purs","end":[21,50]},{"Ident":"labeledField"}]},{"TypeRef":[{"start":[21,52],"name":"docs/Examples/Upload.example.purs","end":[21,69]},"RequiredField",null]}]},"eiImportedAs":null},{"eiModule":["Lumi","Components","Text"],"eiImportType":{"Explicit":[{"ValueRef":[{"start":[22,30],"name":"docs/Examples/Upload.example.purs","end":[22,33]},{"Ident":"h2_"}]}]},"eiImportedAs":null},{"eiModule":["Lumi","Components","Upload"],"eiImportType":{"Explicit":[{"TypeRef":[{"start":[23,32],"name":"docs/Examples/Upload.example.purs","end":[23,42]},"FileId",null]},{"TypeRef":[{"start":[23,44],"name":"docs/Examples/Upload.example.purs","end":[23,56]},"FileName",null]},{"TypeRef":[{"start":[23,58],"name":"docs/Examples/Upload.example.purs","end":[23,75]},"UploadVariant",null]},{"ValueRef":[{"start":[23,77],"name":"docs/Examples/Upload.example.purs","end":[23,85]},{"Ident":"defaults"}]},{"ValueRef":[{"start":[23,87],"name":"docs/Examples/Upload.example.purs","end":[23,93]},{"Ident":"upload"}]}]},"eiImportedAs":null},{"eiModule":["Lumi","Components","Example"],"eiImportType":{"Explicit":[{"ValueRef":[{"start":[24,33],"name":"docs/Examples/Upload.example.purs","end":[24,40]},{"Ident":"example"}]}]},"eiImportedAs":null},{"eiModule":["React","Basic"],"eiImportType":{"Explicit":[{"TypeRef":[{"start":[25,21],"name":"docs/Examples/Upload.example.purs","end":[25,30]},"Component",[]]},{"TypeRef":[{"start":[25,32],"name":"docs/Examples/Upload.example.purs","end":[25,35]},"JSX",[]]},{"ValueRef":[{"start":[25,37],"name":"docs/Examples/Upload.example.purs","end":[25,52]},{"Ident":"createComponent"}]},{"ValueRef":[{"start":[25,54],"name":"docs/Examples/Upload.example.purs","end":[25,58]},{"Ident":"make"}]}]},"eiImportedAs":null},{"eiModule":["React","Basic","DOM"],"eiImportType":{"Implicit":[]},"eiImportedAs":["R"]},{"eiModule":["React","Basic","DOM","Events"],"eiImportType":{"Explicit":[{"ValueRef":[{"start":[27,32],"name":"docs/Examples/Upload.example.purs","end":[27,45]},{"Ident":"targetChecked"}]}]},"eiImportedAs":null},{"eiModule":["React","Basic","Events"],"eiImportType":{"Explicit":[{"ValueRef":[{"start":[28,28],"name":"docs/Examples/Upload.example.purs","end":[28,35]},{"Ident":"handler"}]},{"ValueRef":[{"start":[28,37],"name":"docs/Examples/Upload.example.purs","end":[28,45]},{"Ident":"handler_"}]}]},"eiImportedAs":null},{"eiModule":["Unsafe","Coerce"],"eiImportType":{"Explicit":[{"ValueRef":[{"start":[29,23],"name":"docs/Examples/Upload.example.purs","end":[29,35]},{"Ident":"unsafeCoerce"}]}]},"eiImportedAs":null},{"eiModule":["Web","File","File"],"eiImportType":{"Implicit":[]},"eiImportedAs":["File"]}],"efFixities":[],"efTypeFixities":[],"efDeclarations":[{"EDValue":{"edValueName":{"Ident":"component"},"edValueType":{"annotation":[{"start":[32,14],"name":"docs/Examples/Upload.example.purs","end":[32,28]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[32,14],"name":"docs/Examples/Upload.example.purs","end":[32,23]},[]],"tag":"TypeConstructor","contents":[["React","Basic"],"Component"]},{"annotation":[{"start":[32,24],"name":"docs/Examples/Upload.example.purs","end":[32,28]},[]],"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}}},{"EDType":{"edTypeName":"Action","edTypeKind":{"annotation":[{"start":[0,0],"name":"","end":[0,0]},[]],"tag":"NamedKind","contents":[["Prim"],"Type"]},"edTypeDeclarationKind":{"DataType":{"args":[],"ctors":[["NoOp",[]],["SetReadonly",[{"annotation":[{"start":[37,17],"name":"docs/Examples/Upload.example.purs","end":[37,24]},[]],"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]],["ImageEx",[{"annotation":[{"start":[38,14],"name":"docs/Examples/Upload.example.purs","end":[38,26]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[38,14],"name":"docs/Examples/Upload.example.purs","end":[38,19]},[]],"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"annotation":[{"start":[38,20],"name":"docs/Examples/Upload.example.purs","end":[38,26]},[]],"tag":"TypeConstructor","contents":[["Lumi","Components","Upload"],"FileId"]}]}]],["FileEx",[{"annotation":[{"start":[39,13],"name":"docs/Examples/Upload.example.purs","end":[39,25]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[39,13],"name":"docs/Examples/Upload.example.purs","end":[39,18]},[]],"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"annotation":[{"start":[39,19],"name":"docs/Examples/Upload.example.purs","end":[39,25]},[]],"tag":"TypeConstructor","contents":[["Lumi","Components","Upload"],"FileId"]}]}]],["AvatarEx",[{"annotation":[{"start":[40,15],"name":"docs/Examples/Upload.example.purs","end":[40,27]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[40,15],"name":"docs/Examples/Upload.example.purs","end":[40,20]},[]],"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"annotation":[{"start":[40,21],"name":"docs/Examples/Upload.example.purs","end":[40,27]},[]],"tag":"TypeConstructor","contents":[["Lumi","Components","Upload"],"FileId"]}]}]],["StartUpload",[]],["InitializeUploadBuffer",[{"annotation":[{"start":[42,29],"name":"docs/Examples/Upload.example.purs","end":[42,38]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[42,29],"name":"docs/Examples/Upload.example.purs","end":[42,33]},[]],"tag":"TypeConstructor","contents":[["Effect","AVar"],"AVar"]},{"annotation":[{"start":[42,34],"name":"docs/Examples/Upload.example.purs","end":[42,38]},[]],"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]]]}}}},{"EDDataConstructor":{"edDataCtorName":"NoOp","edDataCtorOrigin":"data","edDataCtorTypeCtor":"Action","edDataCtorType":{"annotation":[{"start":[0,0],"name":"","end":[0,0]},[]],"tag":"TypeConstructor","contents":[["Lumi","Components","Examples","Upload"],"Action"]},"edDataCtorFields":[]}},{"EDDataConstructor":{"edDataCtorName":"SetReadonly","edDataCtorOrigin":"data","edDataCtorTypeCtor":"Action","edDataCtorType":{"annotation":[{"start":[0,0],"name":"","end":[0,0]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[0,0],"name":"","end":[0,0]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[0,0],"name":"","end":[0,0]},[]],"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"annotation":[{"start":[37,17],"name":"docs/Examples/Upload.example.purs","end":[37,24]},[]],"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]},{"annotation":[{"start":[0,0],"name":"","end":[0,0]},[]],"tag":"TypeConstructor","contents":[["Lumi","Components","Examples","Upload"],"Action"]}]},"edDataCtorFields":[{"Ident":"value0"}]}},{"EDDataConstructor":{"edDataCtorName":"ImageEx","edDataCtorOrigin":"data","edDataCtorTypeCtor":"Action","edDataCtorType":{"annotation":[{"start":[0,0],"name":"","end":[0,0]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[0,0],"name":"","end":[0,0]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[0,0],"name":"","end":[0,0]},[]],"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"annotation":[{"start":[38,14],"name":"docs/Examples/Upload.example.purs","end":[38,26]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[38,14],"name":"docs/Examples/Upload.example.purs","end":[38,19]},[]],"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"annotation":[{"start":[38,20],"name":"docs/Examples/Upload.example.purs","end":[38,26]},[]],"tag":"TypeConstructor","contents":[["Lumi","Components","Upload"],"FileId"]}]}]},{"annotation":[{"start":[0,0],"name":"","end":[0,0]},[]],"tag":"TypeConstructor","contents":[["Lumi","Components","Examples","Upload"],"Action"]}]},"edDataCtorFields":[{"Ident":"value0"}]}},{"EDDataConstructor":{"edDataCtorName":"FileEx","edDataCtorOrigin":"data","edDataCtorTypeCtor":"Action","edDataCtorType":{"annotation":[{"start":[0,0],"name":"","end":[0,0]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[0,0],"name":"","end":[0,0]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[0,0],"name":"","end":[0,0]},[]],"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"annotation":[{"start":[39,13],"name":"docs/Examples/Upload.example.purs","end":[39,25]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[39,13],"name":"docs/Examples/Upload.example.purs","end":[39,18]},[]],"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"annotation":[{"start":[39,19],"name":"docs/Examples/Upload.example.purs","end":[39,25]},[]],"tag":"TypeConstructor","contents":[["Lumi","Components","Upload"],"FileId"]}]}]},{"annotation":[{"start":[0,0],"name":"","end":[0,0]},[]],"tag":"TypeConstructor","contents":[["Lumi","Components","Examples","Upload"],"Action"]}]},"edDataCtorFields":[{"Ident":"value0"}]}},{"EDDataConstructor":{"edDataCtorName":"AvatarEx","edDataCtorOrigin":"data","edDataCtorTypeCtor":"Action","edDataCtorType":{"annotation":[{"start":[0,0],"name":"","end":[0,0]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[0,0],"name":"","end":[0,0]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[0,0],"name":"","end":[0,0]},[]],"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"annotation":[{"start":[40,15],"name":"docs/Examples/Upload.example.purs","end":[40,27]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[40,15],"name":"docs/Examples/Upload.example.purs","end":[40,20]},[]],"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"annotation":[{"start":[40,21],"name":"docs/Examples/Upload.example.purs","end":[40,27]},[]],"tag":"TypeConstructor","contents":[["Lumi","Components","Upload"],"FileId"]}]}]},{"annotation":[{"start":[0,0],"name":"","end":[0,0]},[]],"tag":"TypeConstructor","contents":[["Lumi","Components","Examples","Upload"],"Action"]}]},"edDataCtorFields":[{"Ident":"value0"}]}},{"EDDataConstructor":{"edDataCtorName":"StartUpload","edDataCtorOrigin":"data","edDataCtorTypeCtor":"Action","edDataCtorType":{"annotation":[{"start":[0,0],"name":"","end":[0,0]},[]],"tag":"TypeConstructor","contents":[["Lumi","Components","Examples","Upload"],"Action"]},"edDataCtorFields":[]}},{"EDDataConstructor":{"edDataCtorName":"InitializeUploadBuffer","edDataCtorOrigin":"data","edDataCtorTypeCtor":"Action","edDataCtorType":{"annotation":[{"start":[0,0],"name":"","end":[0,0]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[0,0],"name":"","end":[0,0]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[0,0],"name":"","end":[0,0]},[]],"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"annotation":[{"start":[42,29],"name":"docs/Examples/Upload.example.purs","end":[42,38]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[42,29],"name":"docs/Examples/Upload.example.purs","end":[42,33]},[]],"tag":"TypeConstructor","contents":[["Effect","AVar"],"AVar"]},{"annotation":[{"start":[42,34],"name":"docs/Examples/Upload.example.purs","end":[42,38]},[]],"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]},{"annotation":[{"start":[0,0],"name":"","end":[0,0]},[]],"tag":"TypeConstructor","contents":[["Lumi","Components","Examples","Upload"],"Action"]}]},"edDataCtorFields":[{"Ident":"value0"}]}},{"EDValue":{"edValueName":{"Ident":"docs"},"edValueType":{"annotation":[{"start":[44,9],"name":"docs/Examples/Upload.example.purs","end":[44,12]},[]],"tag":"TypeConstructor","contents":[["React","Basic"],"JSX"]}}},{"EDValue":{"edValueName":{"Ident":"debug"},"edValueType":{"annotation":[{"start":[194,10],"name":"docs/Examples/Upload.example.purs","end":[194,46]},[]],"tag":"ForAll","contents":["a",{"annotation":[{"start":[194,20],"name":"docs/Examples/Upload.example.purs","end":[194,46]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[194,20],"name":"docs/Examples/Upload.example.purs","end":[194,46]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[194,27],"name":"docs/Examples/Upload.example.purs","end":[194,29]},[]],"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"annotation":[{"start":[194,20],"name":"docs/Examples/Upload.example.purs","end":[194,26]},[]],"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"annotation":[{"start":[194,30],"name":"docs/Examples/Upload.example.purs","end":[194,46]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[194,30],"name":"docs/Examples/Upload.example.purs","end":[194,46]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[194,32],"name":"docs/Examples/Upload.example.purs","end":[194,34]},[]],"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"annotation":[{"start":[194,30],"name":"docs/Examples/Upload.example.purs","end":[194,31]},[]],"tag":"TypeVar","contents":"a"}]},{"annotation":[{"start":[194,35],"name":"docs/Examples/Upload.example.purs","end":[194,46]},[]],"tag":"TypeApp","contents":[{"annotation":[{"start":[194,35],"name":"docs/Examples/Upload.example.purs","end":[194,41]},[]],"tag":"TypeConstructor","contents":[["Effect"],"Effect"]},{"annotation":[{"start":[194,42],"name":"docs/Examples/Upload.example.purs","end":[194,46]},[]],"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]},0]}}}],"efSourceSpan":{"start":[1,1],"name":"docs/Examples/Upload.example.purs","end":[195,40]}}')}}]);