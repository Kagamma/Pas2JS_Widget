var pas = {};

var rtl = {

  version: 20101,

  quiet: false,
  debug_load_units: false,
  debug_rtti: false,

  $res : {},

  debug: function(){
    if (rtl.quiet || !console || !console.log) return;
    console.log(arguments);
  },

  error: function(s){
    rtl.debug('Error: ',s);
    throw s;
  },

  warn: function(s){
    rtl.debug('Warn: ',s);
  },

  checkVersion: function(v){
    if (rtl.version != v) throw "expected rtl version "+v+", but found "+rtl.version;
  },

  hiInt: Math.pow(2,53),

  hasString: function(s){
    return rtl.isString(s) && (s.length>0);
  },

  isArray: function(a) {
    return Array.isArray(a);
  },

  isFunction: function(f){
    return typeof(f)==="function";
  },

  isModule: function(m){
    return rtl.isObject(m) && rtl.hasString(m.$name) && (pas[m.$name]===m);
  },

  isImplementation: function(m){
    return rtl.isObject(m) && rtl.isModule(m.$module) && (m.$module.$impl===m);
  },

  isNumber: function(n){
    return typeof(n)==="number";
  },

  isObject: function(o){
    var s=typeof(o);
    return (typeof(o)==="object") && (o!=null);
  },

  isString: function(s){
    return typeof(s)==="string";
  },

  getNumber: function(n){
    return typeof(n)==="number"?n:NaN;
  },

  getChar: function(c){
    return ((typeof(c)==="string") && (c.length===1)) ? c : "";
  },

  getObject: function(o){
    return ((typeof(o)==="object") || (typeof(o)==='function')) ? o : null;
  },

  isTRecord: function(type){
    return (rtl.isObject(type) && type.hasOwnProperty('$new') && (typeof(type.$new)==='function'));
  },

  isPasClass: function(type){
    return (rtl.isObject(type) && type.hasOwnProperty('$classname') && rtl.isObject(type.$module));
  },

  isPasClassInstance: function(type){
    return (rtl.isObject(type) && rtl.isPasClass(type.$class));
  },

  hexStr: function(n,digits){
    return ("000000000000000"+n.toString(16).toUpperCase()).slice(-digits);
  },

  m_loading: 0,
  m_loading_intf: 1,
  m_intf_loaded: 2,
  m_loading_impl: 3, // loading all used unit
  m_initializing: 4, // running initialization
  m_initialized: 5,

  module: function(module_name, intfuseslist, intfcode, impluseslist){
    if (rtl.debug_load_units) rtl.debug('rtl.module name="'+module_name+'" intfuses='+intfuseslist+' impluses='+impluseslist);
    if (!rtl.hasString(module_name)) rtl.error('invalid module name "'+module_name+'"');
    if (!rtl.isArray(intfuseslist)) rtl.error('invalid interface useslist of "'+module_name+'"');
    if (!rtl.isFunction(intfcode)) rtl.error('invalid interface code of "'+module_name+'"');
    if (!(impluseslist==undefined) && !rtl.isArray(impluseslist)) rtl.error('invalid implementation useslist of "'+module_name+'"');

    if (pas[module_name])
      rtl.error('module "'+module_name+'" is already registered');

    var r = Object.create(rtl.tSectionRTTI);
    var module = r.$module = pas[module_name] = {
      $name: module_name,
      $intfuseslist: intfuseslist,
      $impluseslist: impluseslist,
      $state: rtl.m_loading,
      $intfcode: intfcode,
      $implcode: null,
      $impl: null,
      $rtti: r
    };
    if (impluseslist) module.$impl = {
          $module: module,
          $rtti: r
        };
  },

  exitcode: 0,

  run: function(module_name){
    try {
      if (!rtl.hasString(module_name)) module_name='program';
      if (rtl.debug_load_units) rtl.debug('rtl.run module="'+module_name+'"');
      rtl.initRTTI();
      var module = pas[module_name];
      if (!module) rtl.error('rtl.run module "'+module_name+'" missing');
      rtl.loadintf(module);
      rtl.loadimpl(module);
      if ((module_name=='program') || (module_name=='library')){
        if (rtl.debug_load_units) rtl.debug('running $main');
        var r = pas[module_name].$main();
        if (rtl.isNumber(r)) rtl.exitcode = r;
      }
    } catch(re) {
      if (!rtl.showUncaughtExceptions) {
        throw re
      } else {  
        if (!rtl.handleUncaughtException(re)) {
          rtl.showException(re);
          rtl.exitcode = 216;
        }  
      }
    } 
    return rtl.exitcode;
  },
  
  showException : function (re) {
    var errMsg = rtl.hasString(re.$classname) ? re.$classname : '';
    errMsg +=  ((errMsg) ? ': ' : '') + (re.hasOwnProperty('fMessage') ? re.fMessage : re);
    alert('Uncaught Exception : '+errMsg);
  },

  handleUncaughtException: function (e) {
    if (rtl.onUncaughtException) {
      try {
        rtl.onUncaughtException(e);
        return true;
      } catch (ee) {
        return false; 
      }
    } else {
      return false;
    }
  },

  loadintf: function(module){
    if (module.$state>rtl.m_loading_intf) return; // already finished
    if (rtl.debug_load_units) rtl.debug('loadintf: "'+module.$name+'"');
    if (module.$state===rtl.m_loading_intf)
      rtl.error('unit cycle detected "'+module.$name+'"');
    module.$state=rtl.m_loading_intf;
    // load interfaces of interface useslist
    rtl.loaduseslist(module,module.$intfuseslist,rtl.loadintf);
    // run interface
    if (rtl.debug_load_units) rtl.debug('loadintf: run intf of "'+module.$name+'"');
    module.$intfcode(module.$intfuseslist);
    // success
    module.$state=rtl.m_intf_loaded;
    // Note: units only used in implementations are not yet loaded (not even their interfaces)
  },

  loaduseslist: function(module,useslist,f){
    if (useslist==undefined) return;
    var len = useslist.length;
    for (var i = 0; i<len; i++) {
      var unitname=useslist[i];
      if (rtl.debug_load_units) rtl.debug('loaduseslist of "'+module.$name+'" uses="'+unitname+'"');
      if (pas[unitname]==undefined)
        rtl.error('module "'+module.$name+'" misses "'+unitname+'"');
      f(pas[unitname]);
    }
  },

  loadimpl: function(module){
    if (module.$state>=rtl.m_loading_impl) return; // already processing
    if (module.$state<rtl.m_intf_loaded) rtl.error('loadimpl: interface not loaded of "'+module.$name+'"');
    if (rtl.debug_load_units) rtl.debug('loadimpl: load uses of "'+module.$name+'"');
    module.$state=rtl.m_loading_impl;
    // load interfaces of implementation useslist
    rtl.loaduseslist(module,module.$impluseslist,rtl.loadintf);
    // load implementation of interfaces useslist
    rtl.loaduseslist(module,module.$intfuseslist,rtl.loadimpl);
    // load implementation of implementation useslist
    rtl.loaduseslist(module,module.$impluseslist,rtl.loadimpl);
    // Note: At this point all interfaces used by this unit are loaded. If
    //   there are implementation uses cycles some used units might not yet be
    //   initialized. This is by design.
    // run implementation
    if (rtl.debug_load_units) rtl.debug('loadimpl: run impl of "'+module.$name+'"');
    if (rtl.isFunction(module.$implcode)) module.$implcode(module.$impluseslist);
    // run initialization
    if (rtl.debug_load_units) rtl.debug('loadimpl: run init of "'+module.$name+'"');
    module.$state=rtl.m_initializing;
    if (rtl.isFunction(module.$init)) module.$init();
    // unit initialized
    module.$state=rtl.m_initialized;
  },

  createCallback: function(scope, fn){
    var cb;
    if (typeof(fn)==='string'){
      if (!scope.hasOwnProperty('$events')) scope.$events = {};
      cb = scope.$events[fn];
      if (cb) return cb;
      scope.$events[fn] = cb = function(){
        return scope[fn].apply(scope,arguments);
      };
    } else {
      cb = function(){
        return fn.apply(scope,arguments);
      };
    };
    cb.scope = scope;
    cb.fn = fn;
    return cb;
  },

  createSafeCallback: function(scope, fn){
    var cb;
    if (typeof(fn)==='string'){
      if (!scope.hasOwnProperty('$events')) scope.$events = {};
      cb = scope.$events[fn];
      if (cb) return cb;
      scope.$events[fn] = cb = function(){
        try{
          return scope[fn].apply(scope,arguments);
        } catch (err) {
          if (!rtl.handleUncaughtException(err)) throw err;
        }
      };
    } else {
      cb = function(){
        try{
          return fn.apply(scope,arguments);
        } catch (err) {
          if (!rtl.handleUncaughtException(err)) throw err;
        }
      };
    };
    cb.scope = scope;
    cb.fn = fn;
    return cb;
  },

  eqCallback: function(a,b){
    // can be a function or a function wrapper
    if (a===b){
      return true;
    } else {
      return (a!=null) && (b!=null) && (a.fn) && (a.scope===b.scope) && (a.fn===b.fn);
    }
  },

  initStruct: function(c,parent,name){
    if ((parent.$module) && (parent.$module.$impl===parent)) parent=parent.$module;
    c.$parent = parent;
    if (rtl.isModule(parent)){
      c.$module = parent;
      c.$name = name;
    } else {
      c.$module = parent.$module;
      c.$name = parent.$name+'.'+name;
    };
    return parent;
  },

  initClass: function(c,parent,name,initfn,rttiname){
    parent[name] = c;
    c.$class = c; // Note: o.$class === Object.getPrototypeOf(o)
    c.$classname = rttiname?rttiname:name;
    parent = rtl.initStruct(c,parent,name);
    c.$fullname = parent.$name+'.'+name;
    // rtti
    if (rtl.debug_rtti) rtl.debug('initClass '+c.$fullname);
    var t = c.$module.$rtti.$Class(c.$classname,{ "class": c });
    c.$rtti = t;
    if (rtl.isObject(c.$ancestor)) t.ancestor = c.$ancestor.$rtti;
    if (!t.ancestor) t.ancestor = null;
    // init members
    initfn.call(c);
  },

  createClass: function(parent,name,ancestor,initfn,rttiname){
    // create a normal class,
    // ancestor must be null or a normal class,
    // the root ancestor can be an external class
    var c = null;
    if (ancestor != null){
      c = Object.create(ancestor);
      c.$ancestor = ancestor;
      // Note:
      // if root is an "object" then c.$ancestor === Object.getPrototypeOf(c)
      // if root is a "function" then c.$ancestor === c.__proto__, Object.getPrototypeOf(c) returns the root
    } else {
      c = { $ancestor: null };
      c.$create = function(fn,args){
        if (args == undefined) args = [];
        var o = Object.create(this);
        o.$init();
        try{
          if (typeof(fn)==="string"){
            o[fn].apply(o,args);
          } else {
            fn.apply(o,args);
          };
          o.AfterConstruction();
        } catch($e){
          // do not call BeforeDestruction
          if (o.Destroy) o.Destroy();
          o.$final();
          throw $e;
        }
        return o;
      };
      c.$destroy = function(fnname){
        this.BeforeDestruction();
        if (this[fnname]) this[fnname]();
        this.$final();
      };
    };
    rtl.initClass(c,parent,name,initfn,rttiname);
  },

  createClassExt: function(parent,name,ancestor,newinstancefnname,initfn,rttiname){
    // Create a class using an external ancestor.
    // If newinstancefnname is given, use that function to create the new object.
    // If exist call BeforeDestruction and AfterConstruction.
    var isFunc = rtl.isFunction(ancestor);
    var c = null;
    if (isFunc){
      // create pascal class descendent from JS function
      c = Object.create(ancestor.prototype);
      c.$ancestorfunc = ancestor;
      c.$ancestor = null; // no pascal ancestor
    } else if (ancestor.$func){
      // create pascal class descendent from a pascal class descendent of a JS function
      isFunc = true;
      c = Object.create(ancestor);
      c.$ancestor = ancestor;
    } else {
      c = Object.create(ancestor);
      c.$ancestor = null; // no pascal ancestor
    }
    c.$create = function(fn,args){
      if (args == undefined) args = [];
      var o = null;
      if (newinstancefnname.length>0){
        o = this[newinstancefnname](fn,args);
      } else if(isFunc) {
        o = new this.$func(args);
      } else {
        o = Object.create(c);
      }
      if (o.$init) o.$init();
      try{
        if (typeof(fn)==="string"){
          this[fn].apply(o,args);
        } else {
          fn.apply(o,args);
        };
        if (o.AfterConstruction) o.AfterConstruction();
      } catch($e){
        // do not call BeforeDestruction
        if (o.Destroy) o.Destroy();
        if (o.$final) o.$final();
        throw $e;
      }
      return o;
    };
    c.$destroy = function(fnname){
      if (this.BeforeDestruction) this.BeforeDestruction();
      if (this[fnname]) this[fnname]();
      if (this.$final) this.$final();
    };
    rtl.initClass(c,parent,name,initfn,rttiname);
    if (isFunc){
      function f(){}
      f.prototype = c;
      c.$func = f;
    }
  },

  createHelper: function(parent,name,ancestor,initfn,rttiname){
    // create a helper,
    // ancestor must be null or a helper,
    var c = null;
    if (ancestor != null){
      c = Object.create(ancestor);
      c.$ancestor = ancestor;
      // c.$ancestor === Object.getPrototypeOf(c)
    } else {
      c = { $ancestor: null };
    };
    parent[name] = c;
    c.$class = c; // Note: o.$class === Object.getPrototypeOf(o)
    c.$classname = rttiname?rttiname:name;
    parent = rtl.initStruct(c,parent,name);
    c.$fullname = parent.$name+'.'+name;
    // rtti
    var t = c.$module.$rtti.$Helper(c.$classname,{ "helper": c });
    c.$rtti = t;
    if (rtl.isObject(ancestor)) t.ancestor = ancestor.$rtti;
    if (!t.ancestor) t.ancestor = null;
    // init members
    initfn.call(c);
  },

  tObjectDestroy: "Destroy",

  free: function(obj,name){
    if (obj[name]==null) return null;
    obj[name].$destroy(rtl.tObjectDestroy);
    obj[name]=null;
  },

  freeLoc: function(obj){
    if (obj==null) return null;
    obj.$destroy(rtl.tObjectDestroy);
    return null;
  },

  hideProp: function(o,p,v){
    Object.defineProperty(o,p, {
      enumerable: false,
      configurable: true,
      writable: true
    });
    if(arguments.length>2){ o[p]=v; }
  },

  recNewT: function(parent,name,initfn,full){
    // create new record type
    var t = {};
    if (parent) parent[name] = t;
    var h = rtl.hideProp;
    if (full){
      rtl.initStruct(t,parent,name);
      t.$record = t;
      h(t,'$record');
      h(t,'$name');
      h(t,'$parent');
      h(t,'$module');
      h(t,'$initSpec');
    }
    initfn.call(t);
    if (!t.$new){
      t.$new = function(){ return Object.create(t); };
    }
    t.$clone = function(r){ return t.$new().$assign(r); };
    h(t,'$new');
    h(t,'$clone');
    h(t,'$eq');
    h(t,'$assign');
    return t;
  },

  is: function(instance,type){
    return type.isPrototypeOf(instance) || (instance===type);
  },

  isExt: function(instance,type,mode){
    // mode===1 means instance must be a Pascal class instance
    // mode===2 means instance must be a Pascal class
    // Notes:
    // isPrototypeOf and instanceof return false on equal
    // isPrototypeOf does not work for Date.isPrototypeOf(new Date())
    //   so if isPrototypeOf is false test with instanceof
    // instanceof needs a function on right side
    if (instance == null) return false; // Note: ==null checks for undefined too
    if ((typeof(type) !== 'object') && (typeof(type) !== 'function')) return false;
    if (instance === type){
      if (mode===1) return false;
      if (mode===2) return rtl.isPasClass(instance);
      return true;
    }
    if (type.isPrototypeOf && type.isPrototypeOf(instance)){
      if (mode===1) return rtl.isPasClassInstance(instance);
      if (mode===2) return rtl.isPasClass(instance);
      return true;
    }
    if ((typeof type == 'function') && (instance instanceof type)) return true;
    return false;
  },

  Exception: null,
  EInvalidCast: null,
  EAbstractError: null,
  ERangeError: null,
  EIntOverflow: null,
  EPropWriteOnly: null,

  raiseE: function(typename){
    var t = rtl[typename];
    if (t==null){
      var mod = pas.SysUtils;
      if (!mod) mod = pas.sysutils;
      if (mod){
        t = mod[typename];
        if (!t) t = mod[typename.toLowerCase()];
        if (!t) t = mod['Exception'];
        if (!t) t = mod['exception'];
      }
    }
    if (t){
      if (t.Create){
        throw t.$create("Create");
      } else if (t.create){
        throw t.$create("create");
      }
    }
    if (typename === "EInvalidCast") throw "invalid type cast";
    if (typename === "EAbstractError") throw "Abstract method called";
    if (typename === "ERangeError") throw "range error";
    throw typename;
  },

  as: function(instance,type){
    if((instance === null) || rtl.is(instance,type)) return instance;
    rtl.raiseE("EInvalidCast");
  },

  asExt: function(instance,type,mode){
    if((instance === null) || rtl.isExt(instance,type,mode)) return instance;
    rtl.raiseE("EInvalidCast");
  },

  createInterface: function(module, name, guid, fnnames, ancestor, initfn){
    //console.log('createInterface name="'+name+'" guid="'+guid+'" names='+fnnames);
    var i = ancestor?Object.create(ancestor):{};
    module[name] = i;
    i.$module = module;
    i.$name = name;
    i.$fullname = module.$name+'.'+name;
    i.$guid = guid;
    i.$guidr = null;
    i.$names = fnnames?fnnames:[];
    if (rtl.isFunction(initfn)){
      // rtti
      if (rtl.debug_rtti) rtl.debug('createInterface '+i.$fullname);
      var t = i.$module.$rtti.$Interface(name,{ "interface": i, module: module });
      i.$rtti = t;
      if (ancestor) t.ancestor = ancestor.$rtti;
      if (!t.ancestor) t.ancestor = null;
      initfn.call(i);
    }
    return i;
  },

  strToGUIDR: function(s,g){
    var p = 0;
    function n(l){
      var h = s.substr(p,l);
      p+=l;
      return parseInt(h,16);
    }
    p+=1; // skip {
    g.D1 = n(8);
    p+=1; // skip -
    g.D2 = n(4);
    p+=1; // skip -
    g.D3 = n(4);
    p+=1; // skip -
    if (!g.D4) g.D4=[];
    g.D4[0] = n(2);
    g.D4[1] = n(2);
    p+=1; // skip -
    for(var i=2; i<8; i++) g.D4[i] = n(2);
    return g;
  },

  guidrToStr: function(g){
    if (g.$intf) return g.$intf.$guid;
    var h = rtl.hexStr;
    var s='{'+h(g.D1,8)+'-'+h(g.D2,4)+'-'+h(g.D3,4)+'-'+h(g.D4[0],2)+h(g.D4[1],2)+'-';
    for (var i=2; i<8; i++) s+=h(g.D4[i],2);
    s+='}';
    return s;
  },

  createTGUID: function(guid){
    var TGuid = (pas.System)?pas.System.TGuid:pas.system.tguid;
    var g = rtl.strToGUIDR(guid,TGuid.$new());
    return g;
  },

  getIntfGUIDR: function(intfTypeOrVar){
    if (!intfTypeOrVar) return null;
    if (!intfTypeOrVar.$guidr){
      var g = rtl.createTGUID(intfTypeOrVar.$guid);
      if (!intfTypeOrVar.hasOwnProperty('$guid')) intfTypeOrVar = Object.getPrototypeOf(intfTypeOrVar);
      g.$intf = intfTypeOrVar;
      intfTypeOrVar.$guidr = g;
    }
    return intfTypeOrVar.$guidr;
  },

  addIntf: function (aclass, intf, map){
    function jmp(fn){
      if (typeof(fn)==="function"){
        return function(){ return fn.apply(this.$o,arguments); };
      } else {
        return function(){ rtl.raiseE('EAbstractError'); };
      }
    }
    if(!map) map = {};
    var t = intf;
    var item = Object.create(t);
    if (!aclass.hasOwnProperty('$intfmaps')) aclass.$intfmaps = {};
    aclass.$intfmaps[intf.$guid] = item;
    do{
      var names = t.$names;
      if (!names) break;
      for (var i=0; i<names.length; i++){
        var intfname = names[i];
        var fnname = map[intfname];
        if (!fnname) fnname = intfname;
        //console.log('addIntf: intftype='+t.$name+' index='+i+' intfname="'+intfname+'" fnname="'+fnname+'" old='+typeof(item[intfname]));
        item[intfname] = jmp(aclass[fnname]);
      }
      t = Object.getPrototypeOf(t);
    }while(t!=null);
  },

  getIntfG: function (obj, guid, query){
    if (!obj) return null;
    //console.log('getIntfG: obj='+obj.$classname+' guid='+guid+' query='+query);
    // search
    var maps = obj.$intfmaps;
    if (!maps) return null;
    var item = maps[guid];
    if (!item) return null;
    // check delegation
    //console.log('getIntfG: obj='+obj.$classname+' guid='+guid+' query='+query+' item='+typeof(item));
    if (typeof item === 'function') return item.call(obj); // delegate. Note: COM contains _AddRef
    // check cache
    var intf = null;
    if (obj.$interfaces){
      intf = obj.$interfaces[guid];
      //console.log('getIntfG: obj='+obj.$classname+' guid='+guid+' cache='+typeof(intf));
    }
    if (!intf){ // intf can be undefined!
      intf = Object.create(item);
      intf.$o = obj;
      if (!obj.$interfaces) obj.$interfaces = {};
      obj.$interfaces[guid] = intf;
    }
    if (typeof(query)==='object'){
      // called by queryIntfT
      var o = null;
      if (intf.QueryInterface(rtl.getIntfGUIDR(query),
          {get:function(){ return o; }, set:function(v){ o=v; }}) === 0){
        return o;
      } else {
        return null;
      }
    } else if(query===2){
      // called by TObject.GetInterfaceByStr
      if (intf.$kind === 'com') intf._AddRef();
    }
    return intf;
  },

  getIntfT: function(obj,intftype){
    return rtl.getIntfG(obj,intftype.$guid);
  },

  queryIntfT: function(obj,intftype){
    return rtl.getIntfG(obj,intftype.$guid,intftype);
  },

  queryIntfIsT: function(obj,intftype){
    var i = rtl.getIntfG(obj,intftype.$guid);
    if (!i) return false;
    if (i.$kind === 'com') i._Release();
    return true;
  },

  asIntfT: function (obj,intftype){
    var i = rtl.getIntfG(obj,intftype.$guid);
    if (i!==null) return i;
    rtl.raiseEInvalidCast();
  },

  intfIsIntfT: function(intf,intftype){
    return (intf!==null) && rtl.queryIntfIsT(intf.$o,intftype);
  },

  intfAsIntfT: function (intf,intftype){
    if (!intf) return null;
    var i = rtl.getIntfG(intf.$o,intftype.$guid);
    if (i) return i;
    rtl.raiseEInvalidCast();
  },

  intfIsClass: function(intf,classtype){
    return (intf!=null) && (rtl.is(intf.$o,classtype));
  },

  intfAsClass: function(intf,classtype){
    if (intf==null) return null;
    return rtl.as(intf.$o,classtype);
  },

  intfToClass: function(intf,classtype){
    if ((intf!==null) && rtl.is(intf.$o,classtype)) return intf.$o;
    return null;
  },

  // interface reference counting
  intfRefs: { // base object for temporary interface variables
    ref: function(id,intf){
      // called for temporary interface references needing delayed release
      var old = this[id];
      //console.log('rtl.intfRefs.ref: id='+id+' old="'+(old?old.$name:'null')+'" intf="'+(intf?intf.$name:'null')+' $o='+(intf?intf.$o:'null'));
      if (old){
        // called again, e.g. in a loop
        delete this[id];
        old._Release(); // may fail
      }
      if(intf) {
        this[id]=intf;
      }
      return intf;
    },
    free: function(){
      //console.log('rtl.intfRefs.free...');
      for (var id in this){
        if (this.hasOwnProperty(id)){
          var intf = this[id];
          if (intf){
            //console.log('rtl.intfRefs.free: id='+id+' '+intf.$name+' $o='+intf.$o.$classname);
            intf._Release();
          }
        }
      }
    }
  },

  createIntfRefs: function(){
    //console.log('rtl.createIntfRefs');
    return Object.create(rtl.intfRefs);
  },

  setIntfP: function(path,name,value,skipAddRef){
    var old = path[name];
    //console.log('rtl.setIntfP path='+path+' name='+name+' old="'+(old?old.$name:'null')+'" value="'+(value?value.$name:'null')+'"');
    if (old === value) return;
    if (old !== null){
      path[name]=null;
      old._Release();
    }
    if (value !== null){
      if (!skipAddRef) value._AddRef();
      path[name]=value;
    }
  },

  setIntfL: function(old,value,skipAddRef){
    //console.log('rtl.setIntfL old="'+(old?old.$name:'null')+'" value="'+(value?value.$name:'null')+'"');
    if (old !== value){
      if (value!==null){
        if (!skipAddRef) value._AddRef();
      }
      if (old!==null){
        old._Release();  // Release after AddRef, to avoid double Release if Release creates an exception
      }
    } else if (skipAddRef){
      if (old!==null){
        old._Release();  // value has an AddRef
      }
    }
    return value;
  },

  _AddRef: function(intf){
    //if (intf) console.log('rtl._AddRef intf="'+(intf?intf.$name:'null')+'"');
    if (intf) intf._AddRef();
    return intf;
  },

  _Release: function(intf){
    //if (intf) console.log('rtl._Release intf="'+(intf?intf.$name:'null')+'"');
    if (intf) intf._Release();
    return intf;
  },

  trunc: function(a){
    return a<0 ? Math.ceil(a) : Math.floor(a);
  },

  checkMethodCall: function(obj,type){
    if (rtl.isObject(obj) && rtl.is(obj,type)) return;
    rtl.raiseE("EInvalidCast");
  },

  oc: function(i){
    // overflow check integer
    if ((Math.floor(i)===i) && (i>=-0x1fffffffffffff) && (i<=0x1fffffffffffff)) return i;
    rtl.raiseE('EIntOverflow');
  },

  rc: function(i,minval,maxval){
    // range check integer
    if ((Math.floor(i)===i) && (i>=minval) && (i<=maxval)) return i;
    rtl.raiseE('ERangeError');
  },

  rcc: function(c,minval,maxval){
    // range check char
    if ((typeof(c)==='string') && (c.length===1)){
      var i = c.charCodeAt(0);
      if ((i>=minval) && (i<=maxval)) return c;
    }
    rtl.raiseE('ERangeError');
  },

  rcSetCharAt: function(s,index,c){
    // range check setCharAt
    if ((typeof(s)!=='string') || (index<0) || (index>=s.length)) rtl.raiseE('ERangeError');
    return rtl.setCharAt(s,index,c);
  },

  rcCharAt: function(s,index){
    // range check charAt
    if ((typeof(s)!=='string') || (index<0) || (index>=s.length)) rtl.raiseE('ERangeError');
    return s.charAt(index);
  },

  rcArrR: function(arr,index){
    // range check read array
    if (Array.isArray(arr) && (typeof(index)==='number') && (index>=0) && (index<arr.length)){
      if (arguments.length>2){
        // arr,index1,index2,...
        arr=arr[index];
        for (var i=2; i<arguments.length; i++) arr=rtl.rcArrR(arr,arguments[i]);
        return arr;
      }
      return arr[index];
    }
    rtl.raiseE('ERangeError');
  },

  rcArrW: function(arr,index,value){
    // range check write array
    // arr,index1,index2,...,value
    for (var i=3; i<arguments.length; i++){
      arr=rtl.rcArrR(arr,index);
      index=arguments[i-1];
      value=arguments[i];
    }
    if (Array.isArray(arr) && (typeof(index)==='number') && (index>=0) && (index<arr.length)){
      return arr[index]=value;
    }
    rtl.raiseE('ERangeError');
  },

  length: function(arr){
    return (arr == null) ? 0 : arr.length;
  },

  arrayRef: function(a){
    if (a!=null) rtl.hideProp(a,'$pas2jsrefcnt',1);
    return a;
  },

  arraySetLength: function(arr,defaultvalue,newlength){
    var stack = [];
    var s = 9999;
    for (var i=2; i<arguments.length; i++){
      var j = arguments[i];
      if (j==='s'){ s = i-2; }
      else {
        stack.push({ dim:j+0, a:null, i:0, src:null });
      }
    }
    var dimmax = stack.length-1;
    var depth = 0;
    var lastlen = 0;
    var item = null;
    var a = null;
    var src = arr;
    var srclen = 0, oldlen = 0;
    do{
      if (depth>0){
        item=stack[depth-1];
        src = (item.src && item.src.length>item.i)?item.src[item.i]:null;
      }
      if (!src){
        a = [];
        srclen = 0;
        oldlen = 0;
      } else if (src.$pas2jsrefcnt>0 || depth>=s){
        a = [];
        srclen = src.length;
        oldlen = srclen;
      } else {
        a = src;
        srclen = 0;
        oldlen = a.length;
      }
      lastlen = stack[depth].dim;
      a.length = lastlen;
      if (depth>0){
        item.a[item.i]=a;
        item.i++;
        if ((lastlen===0) && (item.i<item.a.length)) continue;
      }
      if (lastlen>0){
        if (depth<dimmax){
          item = stack[depth];
          item.a = a;
          item.i = 0;
          item.src = src;
          depth++;
          continue;
        } else {
          if (srclen>lastlen) srclen=lastlen;
          if (rtl.isArray(defaultvalue)){
            // array of dyn array
            for (var i=0; i<srclen; i++) a[i]=src[i];
            for (var i=oldlen; i<lastlen; i++) a[i]=[];
          } else if (rtl.isObject(defaultvalue)) {
            if (rtl.isTRecord(defaultvalue)){
              // array of record
              for (var i=0; i<srclen; i++) a[i]=defaultvalue.$clone(src[i]);
              for (var i=oldlen; i<lastlen; i++) a[i]=defaultvalue.$new();
            } else {
              // array of set
              for (var i=0; i<srclen; i++) a[i]=rtl.refSet(src[i]);
              for (var i=oldlen; i<lastlen; i++) a[i]={};
            }
          } else {
            for (var i=0; i<srclen; i++) a[i]=src[i];
            for (var i=oldlen; i<lastlen; i++) a[i]=defaultvalue;
          }
        }
      }
      // backtrack
      while ((depth>0) && (stack[depth-1].i>=stack[depth-1].dim)){
        depth--;
      };
      if (depth===0){
        if (dimmax===0) return a;
        return stack[0].a;
      }
    }while (true);
  },

  arrayEq: function(a,b){
    if (a===null) return b===null;
    if (b===null) return false;
    if (a.length!==b.length) return false;
    for (var i=0; i<a.length; i++) if (a[i]!==b[i]) return false;
    return true;
  },

  arrayClone: function(type,src,srcpos,endpos,dst,dstpos){
    // type: 0 for references, "refset" for calling refSet(), a function for new type()
    // src must not be null
    // This function does not range check.
    if(type === 'refSet') {
      for (; srcpos<endpos; srcpos++) dst[dstpos++] = rtl.refSet(src[srcpos]); // ref set
    } else if (rtl.isTRecord(type)){
      for (; srcpos<endpos; srcpos++) dst[dstpos++] = type.$clone(src[srcpos]); // clone record
    }  else {
      for (; srcpos<endpos; srcpos++) dst[dstpos++] = src[srcpos]; // reference
    };
  },

  arrayConcat: function(type){
    // type: see rtl.arrayClone
    var a = [];
    var l = 0;
    for (var i=1; i<arguments.length; i++){
      var src = arguments[i];
      if (src !== null) l+=src.length;
    };
    a.length = l;
    l=0;
    for (var i=1; i<arguments.length; i++){
      var src = arguments[i];
      if (src === null) continue;
      rtl.arrayClone(type,src,0,src.length,a,l);
      l+=src.length;
    };
    return a;
  },

  arrayConcatN: function(){
    var a = null;
    for (var i=0; i<arguments.length; i++){
      var src = arguments[i];
      if (src === null) continue;
      if (a===null){
        a=rtl.arrayRef(src); // Note: concat(a) does not clone
      } else {
        a=a.concat(src);
      }
    };
    return a;
  },

  arrayCopy: function(type, srcarray, index, count){
    // type: see rtl.arrayClone
    // if count is missing, use srcarray.length
    if (srcarray === null) return [];
    if (index < 0) index = 0;
    if (count === undefined) count=srcarray.length;
    var end = index+count;
    if (end>srcarray.length) end = srcarray.length;
    if (index>=end) return [];
    if (type===0){
      return srcarray.slice(index,end);
    } else {
      var a = [];
      a.length = end-index;
      rtl.arrayClone(type,srcarray,index,end,a,0);
      return a;
    }
  },

  arrayInsert: function(item, arr, index){
    if (arr){
      arr.splice(index,0,item);
      return arr;
    } else {
      return [item];
    }
  },

  setCharAt: function(s,index,c){
    return s.substr(0,index)+c+s.substr(index+1);
  },

  getResStr: function(mod,name){
    var rs = mod.$resourcestrings[name];
    return rs.current?rs.current:rs.org;
  },

  createSet: function(){
    var s = {};
    for (var i=0; i<arguments.length; i++){
      if (arguments[i]!=null){
        s[arguments[i]]=true;
      } else {
        var first=arguments[i+=1];
        var last=arguments[i+=1];
        for(var j=first; j<=last; j++) s[j]=true;
      }
    }
    return s;
  },

  cloneSet: function(s){
    var r = {};
    for (var key in s) r[key]=true;
    return r;
  },

  refSet: function(s){
    rtl.hideProp(s,'$shared',true);
    return s;
  },

  includeSet: function(s,enumvalue){
    if (s.$shared) s = rtl.cloneSet(s);
    s[enumvalue] = true;
    return s;
  },

  excludeSet: function(s,enumvalue){
    if (s.$shared) s = rtl.cloneSet(s);
    delete s[enumvalue];
    return s;
  },

  diffSet: function(s,t){
    var r = {};
    for (var key in s) if (!t[key]) r[key]=true;
    return r;
  },

  unionSet: function(s,t){
    var r = {};
    for (var key in s) r[key]=true;
    for (var key in t) r[key]=true;
    return r;
  },

  intersectSet: function(s,t){
    var r = {};
    for (var key in s) if (t[key]) r[key]=true;
    return r;
  },

  symDiffSet: function(s,t){
    var r = {};
    for (var key in s) if (!t[key]) r[key]=true;
    for (var key in t) if (!s[key]) r[key]=true;
    return r;
  },

  eqSet: function(s,t){
    for (var key in s) if (!t[key]) return false;
    for (var key in t) if (!s[key]) return false;
    return true;
  },

  neSet: function(s,t){
    return !rtl.eqSet(s,t);
  },

  leSet: function(s,t){
    for (var key in s) if (!t[key]) return false;
    return true;
  },

  geSet: function(s,t){
    for (var key in t) if (!s[key]) return false;
    return true;
  },

  strSetLength: function(s,newlen){
    var oldlen = s.length;
    if (oldlen > newlen){
      return s.substring(0,newlen);
    } else if (s.repeat){
      // Note: repeat needs ECMAScript6!
      return s+' '.repeat(newlen-oldlen);
    } else {
       while (oldlen<newlen){
         s+=' ';
         oldlen++;
       };
       return s;
    }
  },

  spaceLeft: function(s,width){
    var l=s.length;
    if (l>=width) return s;
    if (s.repeat){
      // Note: repeat needs ECMAScript6!
      return ' '.repeat(width-l) + s;
    } else {
      while (l<width){
        s=' '+s;
        l++;
      };
      return s;
    };
  },

  floatToStr: function(d,w,p){
    // input 1-3 arguments: double, width, precision
    if (arguments.length>2){
      return rtl.spaceLeft(d.toFixed(p),w);
    } else {
	  // exponent width
	  var pad = "";
	  var ad = Math.abs(d);
	  if (((ad>1) && (ad<1.0e+10)) ||  ((ad>1.e-10) && (ad<1))) {
		pad='00';
	  } else if ((ad>1) && (ad<1.0e+100) || (ad<1.e-10)) {
		pad='0';
      }  	
	  if (arguments.length<2) {
	    w=24;		
      } else if (w<9) {
		w=9;
      }		  
      var p = w-8;
      var s=(d>0 ? " " : "" ) + d.toExponential(p);
      s=s.replace(/e(.)/,'E$1'+pad);
      return rtl.spaceLeft(s,w);
    }
  },

  valEnum: function(s, enumType, setCodeFn){
    s = s.toLowerCase();
    for (var key in enumType){
      if((typeof(key)==='string') && (key.toLowerCase()===s)){
        setCodeFn(0);
        return enumType[key];
      }
    }
    setCodeFn(1);
    return 0;
  },

  lw: function(l){
    // fix longword bitwise operation
    return l<0?l+0x100000000:l;
  },

  and: function(a,b){
    var hi = 0x80000000;
    var low = 0x7fffffff;
    var h = (a / hi) & (b / hi);
    var l = (a & low) & (b & low);
    return h*hi + l;
  },

  or: function(a,b){
    var hi = 0x80000000;
    var low = 0x7fffffff;
    var h = (a / hi) | (b / hi);
    var l = (a & low) | (b & low);
    return h*hi + l;
  },

  xor: function(a,b){
    var hi = 0x80000000;
    var low = 0x7fffffff;
    var h = (a / hi) ^ (b / hi);
    var l = (a & low) ^ (b & low);
    return h*hi + l;
  },

  shr: function(a,b){
    if (a<0) a += rtl.hiInt;
    if (a<0x80000000) return a >> b;
    if (b<=0) return a;
    if (b>54) return 0;
    return Math.floor(a / Math.pow(2,b));
  },

  shl: function(a,b){
    if (a<0) a += rtl.hiInt;
    if (b<=0) return a;
    if (b>54) return 0;
    var r = a * Math.pow(2,b);
    if (r <= rtl.hiInt) return r;
    return r % rtl.hiInt;
  },

  initRTTI: function(){
    if (rtl.debug_rtti) rtl.debug('initRTTI');

    // base types
    rtl.tTypeInfo = { name: "tTypeInfo", kind: 0, $module: null, attr: null };
    function newBaseTI(name,kind,ancestor){
      if (!ancestor) ancestor = rtl.tTypeInfo;
      if (rtl.debug_rtti) rtl.debug('initRTTI.newBaseTI "'+name+'" '+kind+' ("'+ancestor.name+'")');
      var t = Object.create(ancestor);
      t.name = name;
      t.kind = kind;
      rtl[name] = t;
      return t;
    };
    function newBaseInt(name,minvalue,maxvalue,ordtype){
      var t = newBaseTI(name,1 /* tkInteger */,rtl.tTypeInfoInteger);
      t.minvalue = minvalue;
      t.maxvalue = maxvalue;
      t.ordtype = ordtype;
      return t;
    };
    newBaseTI("tTypeInfoInteger",1 /* tkInteger */);
    newBaseInt("shortint",-0x80,0x7f,0);
    newBaseInt("byte",0,0xff,1);
    newBaseInt("smallint",-0x8000,0x7fff,2);
    newBaseInt("word",0,0xffff,3);
    newBaseInt("longint",-0x80000000,0x7fffffff,4);
    newBaseInt("longword",0,0xffffffff,5);
    newBaseInt("nativeint",-0x10000000000000,0xfffffffffffff,6);
    newBaseInt("nativeuint",0,0xfffffffffffff,7);
    newBaseTI("char",2 /* tkChar */);
    newBaseTI("string",3 /* tkString */);
    newBaseTI("tTypeInfoEnum",4 /* tkEnumeration */,rtl.tTypeInfoInteger);
    newBaseTI("tTypeInfoSet",5 /* tkSet */);
    newBaseTI("double",6 /* tkDouble */);
    newBaseTI("boolean",7 /* tkBool */);
    newBaseTI("tTypeInfoProcVar",8 /* tkProcVar */);
    newBaseTI("tTypeInfoMethodVar",9 /* tkMethod */,rtl.tTypeInfoProcVar);
    newBaseTI("tTypeInfoArray",10 /* tkArray */);
    newBaseTI("tTypeInfoDynArray",11 /* tkDynArray */);
    newBaseTI("tTypeInfoPointer",15 /* tkPointer */);
    var t = newBaseTI("pointer",15 /* tkPointer */,rtl.tTypeInfoPointer);
    t.reftype = null;
    newBaseTI("jsvalue",16 /* tkJSValue */);
    newBaseTI("tTypeInfoRefToProcVar",17 /* tkRefToProcVar */,rtl.tTypeInfoProcVar);

    // member kinds
    rtl.tTypeMember = { attr: null };
    function newMember(name,kind){
      var m = Object.create(rtl.tTypeMember);
      m.name = name;
      m.kind = kind;
      rtl[name] = m;
    };
    newMember("tTypeMemberField",1); // tmkField
    newMember("tTypeMemberMethod",2); // tmkMethod
    newMember("tTypeMemberProperty",3); // tmkProperty

    // base object for storing members: a simple object
    rtl.tTypeMembers = {};

    // tTypeInfoStruct - base object for tTypeInfoClass, tTypeInfoRecord, tTypeInfoInterface
    var tis = newBaseTI("tTypeInfoStruct",0);
    tis.$addMember = function(name,ancestor,options){
      if (rtl.debug_rtti){
        if (!rtl.hasString(name) || (name.charAt()==='$')) throw 'invalid member "'+name+'", this="'+this.name+'"';
        if (!rtl.is(ancestor,rtl.tTypeMember)) throw 'invalid ancestor "'+ancestor+':'+ancestor.name+'", "'+this.name+'.'+name+'"';
        if ((options!=undefined) && (typeof(options)!='object')) throw 'invalid options "'+options+'", "'+this.name+'.'+name+'"';
      };
      var t = Object.create(ancestor);
      t.name = name;
      this.members[name] = t;
      this.names.push(name);
      if (rtl.isObject(options)){
        for (var key in options) if (options.hasOwnProperty(key)) t[key] = options[key];
      };
      return t;
    };
    tis.addField = function(name,type,options){
      var t = this.$addMember(name,rtl.tTypeMemberField,options);
      if (rtl.debug_rtti){
        if (!rtl.is(type,rtl.tTypeInfo)) throw 'invalid type "'+type+'", "'+this.name+'.'+name+'"';
      };
      t.typeinfo = type;
      this.fields.push(name);
      return t;
    };
    tis.addFields = function(){
      var i=0;
      while(i<arguments.length){
        var name = arguments[i++];
        var type = arguments[i++];
        if ((i<arguments.length) && (typeof(arguments[i])==='object')){
          this.addField(name,type,arguments[i++]);
        } else {
          this.addField(name,type);
        };
      };
    };
    tis.addMethod = function(name,methodkind,params,result,flags,options){
      var t = this.$addMember(name,rtl.tTypeMemberMethod,options);
      t.methodkind = methodkind;
      t.procsig = rtl.newTIProcSig(params,result,flags);
      this.methods.push(name);
      return t;
    };
    tis.addProperty = function(name,flags,result,getter,setter,options){
      var t = this.$addMember(name,rtl.tTypeMemberProperty,options);
      t.flags = flags;
      t.typeinfo = result;
      t.getter = getter;
      t.setter = setter;
      // Note: in options: params, stored, defaultvalue
      t.params = rtl.isArray(t.params) ? rtl.newTIParams(t.params) : null;
      this.properties.push(name);
      if (!rtl.isString(t.stored)) t.stored = "";
      return t;
    };
    tis.getField = function(index){
      return this.members[this.fields[index]];
    };
    tis.getMethod = function(index){
      return this.members[this.methods[index]];
    };
    tis.getProperty = function(index){
      return this.members[this.properties[index]];
    };

    newBaseTI("tTypeInfoRecord",12 /* tkRecord */,rtl.tTypeInfoStruct);
    newBaseTI("tTypeInfoClass",13 /* tkClass */,rtl.tTypeInfoStruct);
    newBaseTI("tTypeInfoClassRef",14 /* tkClassRef */);
    newBaseTI("tTypeInfoInterface",18 /* tkInterface */,rtl.tTypeInfoStruct);
    newBaseTI("tTypeInfoHelper",19 /* tkHelper */,rtl.tTypeInfoStruct);
    newBaseTI("tTypeInfoExtClass",20 /* tkExtClass */,rtl.tTypeInfoClass);
  },

  tSectionRTTI: {
    $module: null,
    $inherited: function(name,ancestor,o){
      if (rtl.debug_rtti){
        rtl.debug('tSectionRTTI.newTI "'+(this.$module?this.$module.$name:"(no module)")
          +'"."'+name+'" ('+ancestor.name+') '+(o?'init':'forward'));
      };
      var t = this[name];
      if (t){
        if (!t.$forward) throw 'duplicate type "'+name+'"';
        if (!ancestor.isPrototypeOf(t)) throw 'typeinfo ancestor mismatch "'+name+'" ancestor="'+ancestor.name+'" t.name="'+t.name+'"';
      } else {
        t = Object.create(ancestor);
        t.name = name;
        t.$module = this.$module;
        this[name] = t;
      }
      if (o){
        delete t.$forward;
        for (var key in o) if (o.hasOwnProperty(key)) t[key]=o[key];
      } else {
        t.$forward = true;
      }
      return t;
    },
    $Scope: function(name,ancestor,o){
      var t=this.$inherited(name,ancestor,o);
      t.members = {};
      t.names = [];
      t.fields = [];
      t.methods = [];
      t.properties = [];
      return t;
    },
    $TI: function(name,kind,o){ var t=this.$inherited(name,rtl.tTypeInfo,o); t.kind = kind; return t; },
    $Int: function(name,o){ return this.$inherited(name,rtl.tTypeInfoInteger,o); },
    $Enum: function(name,o){ return this.$inherited(name,rtl.tTypeInfoEnum,o); },
    $Set: function(name,o){ return this.$inherited(name,rtl.tTypeInfoSet,o); },
    $StaticArray: function(name,o){ return this.$inherited(name,rtl.tTypeInfoArray,o); },
    $DynArray: function(name,o){ return this.$inherited(name,rtl.tTypeInfoDynArray,o); },
    $ProcVar: function(name,o){ return this.$inherited(name,rtl.tTypeInfoProcVar,o); },
    $RefToProcVar: function(name,o){ return this.$inherited(name,rtl.tTypeInfoRefToProcVar,o); },
    $MethodVar: function(name,o){ return this.$inherited(name,rtl.tTypeInfoMethodVar,o); },
    $Record: function(name,o){ return this.$Scope(name,rtl.tTypeInfoRecord,o); },
    $Class: function(name,o){ return this.$Scope(name,rtl.tTypeInfoClass,o); },
    $ClassRef: function(name,o){ return this.$inherited(name,rtl.tTypeInfoClassRef,o); },
    $Pointer: function(name,o){ return this.$inherited(name,rtl.tTypeInfoPointer,o); },
    $Interface: function(name,o){ return this.$Scope(name,rtl.tTypeInfoInterface,o); },
    $Helper: function(name,o){ return this.$Scope(name,rtl.tTypeInfoHelper,o); },
    $ExtClass: function(name,o){ return this.$Scope(name,rtl.tTypeInfoExtClass,o); }
  },

  newTIParam: function(param){
    // param is an array, 0=name, 1=type, 2=optional flags
    var t = {
      name: param[0],
      typeinfo: param[1],
      flags: (rtl.isNumber(param[2]) ? param[2] : 0)
    };
    return t;
  },

  newTIParams: function(list){
    // list: optional array of [paramname,typeinfo,optional flags]
    var params = [];
    if (rtl.isArray(list)){
      for (var i=0; i<list.length; i++) params.push(rtl.newTIParam(list[i]));
    };
    return params;
  },

  newTIProcSig: function(params,result,flags){
    var s = {
      params: rtl.newTIParams(params),
      resulttype: result?result:null,
      flags: flags?flags:0
    };
    return s;
  },

  addResource: function(aRes){
    rtl.$res[aRes.name]=aRes;
  },

  getResource: function(aName){
    var res = rtl.$res[aName];
    if (res !== undefined) {
      return res;
    } else {
      return null;
    }
  },

  getResourceList: function(){
    return Object.keys(rtl.$res);
  }
}

rtl.addResource({name: "unit2", unit: "unit2", format: "application/octet-stream", encoding: "base64", data: "b2JqZWN0IFdGb3JtMzogVFdGb3JtMw0KICBMZWZ0ID0gMzQyDQogIEhlaWdodCA9IDI0MA0KICBUb3AgPSAyNTANCiAgV2lkdGggPSAzMjANCiAgQWxwaGFCbGVuZCA9IEZhbHNlDQogIEFscGhhQmxlbmRWYWx1ZSA9IDI1NQ0KICBDYXB0aW9uID0gJ1dGb3JtMycNCmVuZA0K"});
rtl.addResource({name: "main", unit: "main", format: "application/octet-stream", encoding: "base64", data: "b2JqZWN0IFdGb3JtMTogVFdGb3JtMQ0KICBMZWZ0ID0gMjk0DQogIEhlaWdodCA9IDY3MQ0KICBUb3AgPSAxNzMNCiAgV2lkdGggPSAxMTE3DQogIEFscGhhQmxlbmQgPSBGYWxzZQ0KICBBbHBoYUJsZW5kVmFsdWUgPSAyNTUNCiAgQ2FwdGlvbiA9ICdNYWluRm9ybScNCiAgQ2xpZW50SGVpZ2h0ID0gNjcxDQogIENsaWVudFdpZHRoID0gMTExNw0KICBDb2xvciA9IDMzNTU0NDMNCiAgT25DcmVhdGUgPSBGb3JtQ3JlYXRlDQogIG9iamVjdCBXUGFuZWwxOiBUV1BhbmVsDQogICAgTGVmdCA9IDANCiAgICBIZWlnaHQgPSA0MA0KICAgIFRvcCA9IDANCiAgICBXaWR0aCA9IDExMTcNCiAgICBBbGlnbiA9IGFsVG9wDQogICAgQmV2ZWxPdXRlciA9IGJ2Tm9uZQ0KICAgIENhcHRpb24gPSAnV1BhbmVsMScNCiAgICBDbGllbnRIZWlnaHQgPSA0MA0KICAgIENsaWVudFdpZHRoID0gMTExNw0KICAgIENvbG9yID0gMzM1NTQ0Mw0KICAgIFBhcmVudENvbG9yID0gRmFsc2UNCiAgICBQb3B1cE1lbnUgPSBXUG9wdXBNZW51MQ0KICAgIFRhYk9yZGVyID0gMA0KICAgIG9iamVjdCBXQnV0dG9uMTogVFdCdXR0b24NCiAgICAgIExlZnQgPSA4DQogICAgICBIZWlnaHQgPSAyNQ0KICAgICAgVG9wID0gNw0KICAgICAgV2lkdGggPSA3NQ0KICAgICAgQ2FwdGlvbiA9ICdDT0xPUicNCiAgICAgIENvbG9yID0gNjcxMDc4NA0KICAgICAgVGFiT3JkZXIgPSAwDQogICAgICBPbkNsaWNrID0gV0J1dHRvbjFDbGljaw0KICAgIGVuZA0KICAgIG9iamVjdCBXQnV0dG9uMjogVFdCdXR0b24NCiAgICAgIExlZnQgPSA4OA0KICAgICAgSGVpZ2h0ID0gMjUNCiAgICAgIFRvcCA9IDgNCiAgICAgIFdpZHRoID0gNzUNCiAgICAgIEFscGhhID0gMTAwDQogICAgICBDYXB0aW9uID0gJ05FVyBGT1JNJw0KICAgICAgQ29sb3IgPSA2NzEwNzg0DQogICAgICBUYWJPcmRlciA9IDENCiAgICAgIE9uQ2xpY2sgPSBXQnV0dG9uMkNsaWNrDQogICAgZW5kDQogICAgb2JqZWN0IFdCdXR0b24zOiBUV0J1dHRvbg0KICAgICAgTGVmdCA9IDE2OA0KICAgICAgSGVpZ2h0ID0gMjUNCiAgICAgIFRvcCA9IDcNCiAgICAgIFdpZHRoID0gNzUNCiAgICAgIENhcHRpb24gPSAnQUxQSEEnDQogICAgICBDb2xvciA9IDY3MTA3ODQNCiAgICAgIFRhYk9yZGVyID0gMg0KICAgICAgT25DbGljayA9IFdCdXR0b24zQ2xpY2sNCiAgICBlbmQNCiAgICBvYmplY3QgV0J1dHRvbjQ6IFRXQnV0dG9uDQogICAgICBMZWZ0ID0gMjQ4DQogICAgICBIZWlnaHQgPSAyNQ0KICAgICAgVG9wID0gOA0KICAgICAgV2lkdGggPSA3NQ0KICAgICAgQ2FwdGlvbiA9ICdCVVRUT04gNCcNCiAgICAgIENvbG9yID0gNjcxMDc4NA0KICAgICAgVGFiT3JkZXIgPSAzDQogICAgZW5kDQogICAgb2JqZWN0IGxibFVzZXJOYW1lOiBUV0xhYmVsDQogICAgICBMZWZ0ID0gNzkxDQogICAgICBIZWlnaHQgPSAyMQ0KICAgICAgVG9wID0gMTENCiAgICAgIFdpZHRoID0gMjM3DQogICAgICBBbGlnbm1lbnQgPSB0YUNlbnRlcg0KICAgICAgQW5jaG9ycyA9IFtha1RvcCwgYWtSaWdodF0NCiAgICAgIEF1dG9TaXplID0gRmFsc2UNCiAgICAgIENhcHRpb24gPSAnVEVTVCcNCiAgICAgIEZvbnQuQ29sb3IgPSBjbFllbGxvdw0KICAgICAgRm9udC5IZWlnaHQgPSAtOQ0KICAgICAgUGFyZW50Rm9udCA9IEZhbHNlDQogICAgZW5kDQogICAgb2JqZWN0IFdCdXR0b241OiBUV0J1dHRvbg0KICAgICAgTGVmdCA9IDEwMzYNCiAgICAgIEhlaWdodCA9IDI1DQogICAgICBUb3AgPSA3DQogICAgICBXaWR0aCA9IDc1DQogICAgICBBbmNob3JzID0gW2FrVG9wLCBha1JpZ2h0XQ0KICAgICAgQ2FwdGlvbiA9ICdCVVRUT04gNScNCiAgICAgIENvbG9yID0gNjcxMDc4NA0KICAgICAgVGFiT3JkZXIgPSA0DQogICAgICBPbkNsaWNrID0gV0J1dHRvbjVDbGljaw0KICAgIGVuZA0KICBlbmQNCiAgb2JqZWN0IFdQYW5lbDI6IFRXUGFuZWwNCiAgICBMZWZ0ID0gMA0KICAgIEhlaWdodCA9IDExNg0KICAgIFRvcCA9IDU1NQ0KICAgIFdpZHRoID0gMTExNw0KICAgIEFsaWduID0gYWxCb3R0b20NCiAgICBCZXZlbE91dGVyID0gYnZOb25lDQogICAgQ2FwdGlvbiA9ICdXUGFuZWwyJw0KICAgIENvbG9yID0gMzM1OTc5NQ0KICAgIFBhcmVudENvbG9yID0gRmFsc2UNCiAgICBQb3B1cE1lbnUgPSBXUG9wdXBNZW51MQ0KICAgIFRhYk9yZGVyID0gMQ0KICBlbmQNCiAgb2JqZWN0IFdTcGxpdHRlcjI6IFRXU3BsaXR0ZXINCiAgICBDdXJzb3IgPSBjclZTcGxpdA0KICAgIExlZnQgPSAwDQogICAgSGVpZ2h0ID0gNQ0KICAgIFRvcCA9IDU1MA0KICAgIFdpZHRoID0gMTExNw0KICAgIEFsaWduID0gYWxCb3R0b20NCiAgICBDb2xvciA9IGNsTGltZQ0KICBlbmQNCiAgb2JqZWN0IFdQYW5lbDM6IFRXUGFuZWwNCiAgICBMZWZ0ID0gMA0KICAgIEhlaWdodCA9IDUxMA0KICAgIFRvcCA9IDQwDQogICAgV2lkdGggPSAxMTE3DQogICAgQWxpZ24gPSBhbENsaWVudA0KICAgIEJldmVsT3V0ZXIgPSBidk5vbmUNCiAgICBDYXB0aW9uID0gJ1dQYW5lbDMnDQogICAgQ2xpZW50SGVpZ2h0ID0gNTEwDQogICAgQ2xpZW50V2lkdGggPSAxMTE3DQogICAgQ29sb3IgPSAzMzU1NDYwDQogICAgUGFyZW50Q29sb3IgPSBGYWxzZQ0KICAgIFRhYk9yZGVyID0gMw0KICAgIG9iamVjdCBXUGFuZWw0OiBUV1BhbmVsDQogICAgICBMZWZ0ID0gODIwDQogICAgICBIZWlnaHQgPSA1MTANCiAgICAgIFRvcCA9IDANCiAgICAgIFdpZHRoID0gMjk3DQogICAgICBBbGlnbiA9IGFsUmlnaHQNCiAgICAgIEJldmVsT3V0ZXIgPSBidk5vbmUNCiAgICAgIENhcHRpb24gPSAnV1BhbmVsNCcNCiAgICAgIENsaWVudEhlaWdodCA9IDUxMA0KICAgICAgQ2xpZW50V2lkdGggPSAyOTcNCiAgICAgIENvbG9yID0gY2xOYXZ5DQogICAgICBQYXJlbnRDb2xvciA9IEZhbHNlDQogICAgICBUYWJPcmRlciA9IDANCiAgICAgIG9iamVjdCBXUGFuZWw3OiBUV1BhbmVsDQogICAgICAgIExlZnQgPSAwDQogICAgICAgIEhlaWdodCA9IDI2DQogICAgICAgIFRvcCA9IDANCiAgICAgICAgV2lkdGggPSAyOTcNCiAgICAgICAgQWxpZ24gPSBhbFRvcA0KICAgICAgICBCZXZlbE91dGVyID0gYnZOb25lDQogICAgICAgIENhcHRpb24gPSAnV1BhbmVsNycNCiAgICAgICAgQ2xpZW50SGVpZ2h0ID0gMjYNCiAgICAgICAgQ2xpZW50V2lkdGggPSAyOTcNCiAgICAgICAgQ29sb3IgPSA2NzA0NDYxDQogICAgICAgIFBhcmVudENvbG9yID0gRmFsc2UNCiAgICAgICAgVGFiT3JkZXIgPSAwDQogICAgICAgIG9iamVjdCBXUGFuZWwxMDogVFdQYW5lbA0KICAgICAgICAgIExlZnQgPSAwDQogICAgICAgICAgSGVpZ2h0ID0gMjYNCiAgICAgICAgICBUb3AgPSAwDQogICAgICAgICAgV2lkdGggPSAxNzANCiAgICAgICAgICBBbGlnbiA9IGFsTGVmdA0KICAgICAgICAgIEJldmVsT3V0ZXIgPSBidk5vbmUNCiAgICAgICAgICBDYXB0aW9uID0gJ1dQYW5lbDEwJw0KICAgICAgICAgIENvbG9yID0gY2xHcmVlbg0KICAgICAgICAgIFBhcmVudENvbG9yID0gRmFsc2UNCiAgICAgICAgICBUYWJPcmRlciA9IDANCiAgICAgICAgZW5kDQogICAgICBlbmQNCiAgICAgIG9iamVjdCBXUGFuZWwxMTogVFdQYW5lbA0KICAgICAgICBMZWZ0ID0gOA0KICAgICAgICBIZWlnaHQgPSA4Mg0KICAgICAgICBUb3AgPSA0MA0KICAgICAgICBXaWR0aCA9IDI0Mg0KICAgICAgICBDYXB0aW9uID0gJ1dQYW5lbDExJw0KICAgICAgICBDbGllbnRIZWlnaHQgPSA4Mg0KICAgICAgICBDbGllbnRXaWR0aCA9IDI0Mg0KICAgICAgICBDb2xvciA9IGNsWWVsbG93DQogICAgICAgIFBhcmVudENvbG9yID0gRmFsc2UNCiAgICAgICAgVGFiT3JkZXIgPSAxDQogICAgICAgIG9iamVjdCBXTGFiZWwxOiBUV0xhYmVsDQogICAgICAgICAgTGVmdCA9IDgNCiAgICAgICAgICBIZWlnaHQgPSAyMQ0KICAgICAgICAgIFRvcCA9IDYNCiAgICAgICAgICBXaWR0aCA9IDY2DQogICAgICAgICAgQ2FwdGlvbiA9ICdXTGFiZWwxJw0KICAgICAgICAgIEZvbnQuSGVpZ2h0ID0gLTE2DQogICAgICAgICAgRm9udC5TdHlsZSA9IFtmc0JvbGRdDQogICAgICAgICAgUGFyZW50Rm9udCA9IEZhbHNlDQogICAgICAgIGVuZA0KICAgICAgZW5kDQogICAgICBvYmplY3QgV0ltYWdlMTogVFdJbWFnZQ0KICAgICAgICBMZWZ0ID0gOA0KICAgICAgICBIZWlnaHQgPSAzMDANCiAgICAgICAgVG9wID0gMTM2DQogICAgICAgIFdpZHRoID0gMjAwDQogICAgICAgIFVSTCA9ICdodHRwczovL3BpY3N1bS5waG90b3MvMjAwLzMwMCcNCiAgICAgIGVuZA0KICAgIGVuZA0KICAgIG9iamVjdCBXUGFuZWw1OiBUV1BhbmVsDQogICAgICBMZWZ0ID0gMA0KICAgICAgSGVpZ2h0ID0gNTEwDQogICAgICBUb3AgPSAwDQogICAgICBXaWR0aCA9IDgxNQ0KICAgICAgQWxpZ24gPSBhbENsaWVudA0KICAgICAgQmV2ZWxPdXRlciA9IGJ2Tm9uZQ0KICAgICAgQ2FwdGlvbiA9ICdXUGFuZWw1Jw0KICAgICAgQ2xpZW50SGVpZ2h0ID0gNTEwDQogICAgICBDbGllbnRXaWR0aCA9IDgxNQ0KICAgICAgUG9wdXBNZW51ID0gV1BvcHVwTWVudTENCiAgICAgIFRhYk9yZGVyID0gMQ0KICAgICAgT25DbGljayA9IFdQYW5lbDVDbGljaw0KICAgICAgb2JqZWN0IFdQYW5lbDY6IFRXUGFuZWwNCiAgICAgICAgTGVmdCA9IDANCiAgICAgICAgSGVpZ2h0ID0gMjYNCiAgICAgICAgVG9wID0gMA0KICAgICAgICBXaWR0aCA9IDgxNQ0KICAgICAgICBBbGlnbiA9IGFsVG9wDQogICAgICAgIEJldmVsT3V0ZXIgPSBidk5vbmUNCiAgICAgICAgQ2FwdGlvbiA9ICdXUGFuZWw2Jw0KICAgICAgICBDb2xvciA9IDY3MDQ0NjENCiAgICAgICAgUGFyZW50Q29sb3IgPSBGYWxzZQ0KICAgICAgICBQb3B1cE1lbnUgPSBXUG9wdXBNZW51MQ0KICAgICAgICBUYWJPcmRlciA9IDANCiAgICAgIGVuZA0KICAgICAgb2JqZWN0IFdQYW5lbDg6IFRXUGFuZWwNCiAgICAgICAgTGVmdCA9IDE2DQogICAgICAgIEhlaWdodCA9IDEzNQ0KICAgICAgICBUb3AgPSA0MA0KICAgICAgICBXaWR0aCA9IDEyMA0KICAgICAgICBBbHBoYSA9IDIwMA0KICAgICAgICBDYXB0aW9uID0gJ1dQYW5lbDgnDQogICAgICAgIENvbG9yID0gY2xCbHVlDQogICAgICAgIFBhcmVudENvbG9yID0gRmFsc2UNCiAgICAgICAgVGFiT3JkZXIgPSAxDQogICAgICAgIE9uQ2xpY2sgPSBXUGFuZWw4Q2xpY2sNCiAgICAgIGVuZA0KICAgICAgb2JqZWN0IFdQYW5lbDk6IFRXUGFuZWwNCiAgICAgICAgTGVmdCA9IDcyDQogICAgICAgIEhlaWdodCA9IDIxMA0KICAgICAgICBUb3AgPSA4OA0KICAgICAgICBXaWR0aCA9IDEyMQ0KICAgICAgICBBbHBoYSA9IDE1MA0KICAgICAgICBDYXB0aW9uID0gJ1dQYW5lbDknDQogICAgICAgIENsaWVudEhlaWdodCA9IDIxMA0KICAgICAgICBDbGllbnRXaWR0aCA9IDEyMQ0KICAgICAgICBDb2xvciA9IGNsRnVjaHNpYQ0KICAgICAgICBQYXJlbnRDb2xvciA9IEZhbHNlDQogICAgICAgIFBvcHVwTWVudSA9IFdQb3B1cE1lbnUxDQogICAgICAgIFRhYk9yZGVyID0gMg0KICAgICAgICBPbkNsaWNrID0gV1BhbmVsOUNsaWNrDQogICAgICAgIE9uTW91c2VEb3duID0gV1BhbmVsOU1vdXNlRG93bg0KICAgICAgICBPbk1vdXNlTW92ZSA9IFdQYW5lbDlNb3VzZU1vdmUNCiAgICAgICAgT25Nb3VzZVVwID0gV1BhbmVsOU1vdXNlVXANCiAgICAgICAgb2JqZWN0IFdMYWJlbDI6IFRXTGFiZWwNCiAgICAgICAgICBMZWZ0ID0gMjQNCiAgICAgICAgICBIZWlnaHQgPSAxNQ0KICAgICAgICAgIFRvcCA9IDQ3DQogICAgICAgICAgV2lkdGggPSA2OA0KICAgICAgICAgIENhcHRpb24gPSAnUklHSFQgQ0xJQ0snDQogICAgICAgIGVuZA0KICAgICAgZW5kDQogICAgICBvYmplY3QgV0VkaXQxOiBUV0VkaXQNCiAgICAgICAgTGVmdCA9IDIyNA0KICAgICAgICBIZWlnaHQgPSAyMw0KICAgICAgICBUb3AgPSAzOQ0KICAgICAgICBXaWR0aCA9IDEyOA0KICAgICAgICBUYWJPcmRlciA9IDMNCiAgICAgICAgVGV4dCA9ICdXRWRpdDEnDQogICAgICBlbmQNCiAgICAgIG9iamVjdCBXQ29tYm9Cb3gxOiBUV0NvbWJvQm94DQogICAgICAgIExlZnQgPSAzODQNCiAgICAgICAgSGVpZ2h0ID0gMjMNCiAgICAgICAgVG9wID0gMzkNCiAgICAgICAgV2lkdGggPSAxNTINCiAgICAgICAgSXRlbUhlaWdodCA9IDE1DQogICAgICAgIEl0ZW1zLlN0cmluZ3MgPSAoDQogICAgICAgICAgJ1Rva3lvJw0KICAgICAgICAgICdOZXcgWW9yaycNCiAgICAgICAgICAnTG9zIEFuZ2VsZXMnDQogICAgICAgICAgJ1Nlb3VsJw0KICAgICAgICAgICdMb25kb24nDQogICAgICAgICAgJ1BhcmlzJw0KICAgICAgICAgICdUb3JvbnRvJw0KICAgICAgICApDQogICAgICAgIFRhYk9yZGVyID0gNA0KICAgICAgZW5kDQogICAgICBvYmplY3QgV0xpc3RCb3gxOiBUV0xpc3RCb3gNCiAgICAgICAgTGVmdCA9IDIyNA0KICAgICAgICBIZWlnaHQgPSAyNjQNCiAgICAgICAgVG9wID0gODANCiAgICAgICAgV2lkdGggPSAxNTINCiAgICAgICAgSXRlbUhlaWdodCA9IDE1DQogICAgICAgIEl0ZW1zLlN0cmluZ3MgPSAoDQogICAgICAgICAgJ1Rva3lvJw0KICAgICAgICAgICdOZXcgWW9yaycNCiAgICAgICAgICAnTG9zIEFuZ2VsZXMnDQogICAgICAgICAgJ1Nlb3VsJw0KICAgICAgICAgICdMb25kb24nDQogICAgICAgICAgJ1BhcmlzJw0KICAgICAgICAgICdUb3JvbnRvJw0KICAgICAgICApDQogICAgICAgIFRhYk9yZGVyID0gNQ0KICAgICAgICBPbkNsaWNrID0gV0xpc3RCb3gxQ2xpY2sNCiAgICAgICAgT25TZWxlY3Rpb25DaGFuZ2UgPSBXTGlzdEJveDFTZWxlY3Rpb25DaGFuZ2UNCiAgICAgIGVuZA0KICAgICAgb2JqZWN0IFdNZW1vMTogVFdNZW1vDQogICAgICAgIExlZnQgPSAzOTINCiAgICAgICAgSGVpZ2h0ID0gMTIyDQogICAgICAgIFRvcCA9IDgwDQogICAgICAgIFdpZHRoID0gMTkyDQogICAgICAgIExpbmVzLlN0cmluZ3MgPSAoDQogICAgICAgICAgJ0xvcmVtIElwc3VtIGlzIHNpbXBseSBkdW1teSB0ZXh0IG9mIHRoZSBwcmludGluZyBhbmQgdHlwZXNldHRpbmcgaW5kdXN0cnkuIExvcmVtIElwc3VtIGhhcyBiZWVuIHRoZSBpbmR1c3RyeScncyBzdGFuZGFyZCBkdW1teSB0ZXh0IGV2ZXIgc2luY2UgdGhlIDE1MDBzLCB3aGVuIGFuIHVua25vd24gcHJpbnRlciB0b29rIGEgZ2FsbGV5IG9mIHR5cGUgYW5kIHNjcmFtYmxlZCBpdCB0byBtYWtlIGEgdHlwZSBzcGVjaW1lbiBib29rLiBJdCBoYXMgc3Vydml2ZWQgbm90IG9ubHkgZml2ZSBjZW50dXJpZXMsIGJ1dCBhbHNvIHRoZSBsZWFwIGludG8gZWxlY3Ryb25pYyB0eXBlc2V0dGluZywgcmVtYWluaW5nIGVzc2VudGlhbGx5IHVuY2hhbmdlZC4gSXQgd2FzIHBvcHVsYXJpc2VkIGluIHRoZSAxOTYwcyB3aXRoIHRoZSByZWxlYXNlIG9mIExldHJhc2V0IHNoZWV0cyBjb250YWluaW5nIExvcmVtIElwc3VtIHBhc3NhZ2VzLCBhbmQgbW9yZSByZWNlbnRseSB3aXRoIGRlc2t0b3AgcHVibGlzaGluZyBzb2Z0d2FyZSBsaWtlIEFsZHVzIFBhZ2VNYWtlciBpbmNsdWRpbmcgdmVyc2lvbnMgb2YgTG9yZW0gSXBzdW0uJw0KICAgICAgICApDQogICAgICAgIFRhYk9yZGVyID0gNg0KICAgICAgZW5kDQogICAgICBvYmplY3QgV0NoZWNrYm94MTogVFdDaGVja2JveA0KICAgICAgICBMZWZ0ID0gMzkyDQogICAgICAgIEhlaWdodCA9IDIzDQogICAgICAgIFRvcCA9IDIxNw0KICAgICAgICBXaWR0aCA9IDkwDQogICAgICAgIENhcHRpb24gPSAnV0NoZWNrYm94MScNCiAgICAgICAgRm9udC5Db2xvciA9IGNsWWVsbG93DQogICAgICAgIFBhcmVudEZvbnQgPSBGYWxzZQ0KICAgICAgICBUYWJPcmRlciA9IDcNCiAgICAgIGVuZA0KICAgICAgb2JqZWN0IFdDaGVja2JveDI6IFRXQ2hlY2tib3gNCiAgICAgICAgTGVmdCA9IDM5Mg0KICAgICAgICBIZWlnaHQgPSAyMw0KICAgICAgICBUb3AgPSAyNDANCiAgICAgICAgV2lkdGggPSA5MA0KICAgICAgICBDYXB0aW9uID0gJ1dDaGVja2JveDInDQogICAgICAgIENoZWNrZWQgPSBUcnVlDQogICAgICAgIEZvbnQuQ29sb3IgPSBjbFllbGxvdw0KICAgICAgICBQYXJlbnRGb250ID0gRmFsc2UNCiAgICAgICAgU3RhdGUgPSBjYkNoZWNrZWQNCiAgICAgICAgVGFiT3JkZXIgPSA4DQogICAgICBlbmQNCiAgICAgIG9iamVjdCBXQ2hlY2tib3gzOiBUV0NoZWNrYm94DQogICAgICAgIExlZnQgPSAzOTINCiAgICAgICAgSGVpZ2h0ID0gMjMNCiAgICAgICAgVG9wID0gMjY0DQogICAgICAgIFdpZHRoID0gOTANCiAgICAgICAgQ2FwdGlvbiA9ICdXQ2hlY2tib3gzJw0KICAgICAgICBGb250LkNvbG9yID0gY2xZZWxsb3cNCiAgICAgICAgUGFyZW50Rm9udCA9IEZhbHNlDQogICAgICAgIFRhYk9yZGVyID0gOQ0KICAgICAgZW5kDQogICAgICBvYmplY3QgV1JhZGlvQnV0dG9uMjogVFdSYWRpb0J1dHRvbg0KICAgICAgICBMZWZ0ID0gMTYNCiAgICAgICAgSGVpZ2h0ID0gMTkNCiAgICAgICAgVG9wID0gMzQ2DQogICAgICAgIFdpZHRoID0gMTAxDQogICAgICAgIENhcHRpb24gPSAnV1JhZGlvQnV0dG9uMicNCiAgICAgICAgRm9udC5Db2xvciA9IGNsU2lsdmVyDQogICAgICAgIFBhcmVudEZvbnQgPSBGYWxzZQ0KICAgICAgICBUYWJPcmRlciA9IDEwDQogICAgICBlbmQNCiAgICAgIG9iamVjdCBXUmFkaW9CdXR0b24xOiBUV1JhZGlvQnV0dG9uDQogICAgICAgIExlZnQgPSAxNg0KICAgICAgICBIZWlnaHQgPSAxOQ0KICAgICAgICBUb3AgPSAzMjANCiAgICAgICAgV2lkdGggPSAxMDENCiAgICAgICAgQ2FwdGlvbiA9ICdXUmFkaW9CdXR0b24xJw0KICAgICAgICBDaGVja2VkID0gVHJ1ZQ0KICAgICAgICBGb250LkNvbG9yID0gY2xTaWx2ZXINCiAgICAgICAgUGFyZW50Rm9udCA9IEZhbHNlDQogICAgICAgIFRhYk9yZGVyID0gMTENCiAgICAgIGVuZA0KICAgICAgb2JqZWN0IFdQYWdlQ29udHJvbDE6IFRXUGFnZUNvbnRyb2wNCiAgICAgICAgTGVmdCA9IDM5Mg0KICAgICAgICBIZWlnaHQgPSAyMDANCiAgICAgICAgVG9wID0gMzA0DQogICAgICAgIFdpZHRoID0gMjAwDQogICAgICAgIEFjdGl2ZVBhZ2UgPSBUYWJTaGVldDENCiAgICAgICAgVGFiSW5kZXggPSAwDQogICAgICAgIFRhYk9yZGVyID0gMTINCiAgICAgICAgb2JqZWN0IFRhYlNoZWV0MTogVFRhYlNoZWV0DQogICAgICAgICAgQ2FwdGlvbiA9ICdUYWJTaGVldDEnDQogICAgICAgICAgQ2xpZW50SGVpZ2h0ID0gMTcyDQogICAgICAgICAgQ2xpZW50V2lkdGggPSAxOTINCiAgICAgICAgICBvYmplY3QgV0J1dHRvbjY6IFRXQnV0dG9uDQogICAgICAgICAgICBMZWZ0ID0gMTgNCiAgICAgICAgICAgIEhlaWdodCA9IDI1DQogICAgICAgICAgICBUb3AgPSAxNg0KICAgICAgICAgICAgV2lkdGggPSA3NQ0KICAgICAgICAgICAgQ2FwdGlvbiA9ICdQYWdlIDEnDQogICAgICAgICAgICBUYWJPcmRlciA9IDANCiAgICAgICAgICBlbmQNCiAgICAgICAgZW5kDQogICAgICAgIG9iamVjdCBUYWJTaGVldDI6IFRUYWJTaGVldA0KICAgICAgICAgIENhcHRpb24gPSAnVGFiU2hlZXQyJw0KICAgICAgICAgIENsaWVudEhlaWdodCA9IDE3Mg0KICAgICAgICAgIENsaWVudFdpZHRoID0gMTkyDQogICAgICAgICAgb2JqZWN0IFdCdXR0b243OiBUV0J1dHRvbg0KICAgICAgICAgICAgTGVmdCA9IDgNCiAgICAgICAgICAgIEhlaWdodCA9IDI1DQogICAgICAgICAgICBUb3AgPSAzMg0KICAgICAgICAgICAgV2lkdGggPSA3NQ0KICAgICAgICAgICAgQ2FwdGlvbiA9ICdQYWdlIDInDQogICAgICAgICAgICBUYWJPcmRlciA9IDANCiAgICAgICAgICBlbmQNCiAgICAgICAgZW5kDQogICAgICAgIG9iamVjdCBUYWJTaGVldDM6IFRUYWJTaGVldA0KICAgICAgICAgIENhcHRpb24gPSAnVGFiU2hlZXQzJw0KICAgICAgICAgIENsaWVudEhlaWdodCA9IDE3Mg0KICAgICAgICAgIENsaWVudFdpZHRoID0gMTkyDQogICAgICAgICAgb2JqZWN0IFdCdXR0b244OiBUV0J1dHRvbg0KICAgICAgICAgICAgTGVmdCA9IDgNCiAgICAgICAgICAgIEhlaWdodCA9IDI1DQogICAgICAgICAgICBUb3AgPSA5NQ0KICAgICAgICAgICAgV2lkdGggPSA3NQ0KICAgICAgICAgICAgQ2FwdGlvbiA9ICdQYWdlIDMnDQogICAgICAgICAgICBUYWJPcmRlciA9IDANCiAgICAgICAgICBlbmQNCiAgICAgICAgZW5kDQogICAgICBlbmQNCiAgICAgIG9iamVjdCBXRmxvYXRFZGl0MTogVFdGbG9hdEVkaXQNCiAgICAgICAgTGVmdCA9IDE2DQogICAgICAgIEhlaWdodCA9IDIzDQogICAgICAgIFRvcCA9IDM5Mg0KICAgICAgICBXaWR0aCA9IDExMg0KICAgICAgICBBbGlnbm1lbnQgPSB0YVJpZ2h0SnVzdGlmeQ0KICAgICAgICBEZWNpbWFsUGxhY2VzID0gMg0KICAgICAgICBUYWJPcmRlciA9IDEzDQogICAgICAgIFRleHQgPSAnMCwwMCcNCiAgICAgIGVuZA0KICAgICAgb2JqZWN0IFdJbnRlZ2VyRWRpdDE6IFRXSW50ZWdlckVkaXQNCiAgICAgICAgTGVmdCA9IDE2DQogICAgICAgIEhlaWdodCA9IDIzDQogICAgICAgIFRvcCA9IDQzMQ0KICAgICAgICBXaWR0aCA9IDExMg0KICAgICAgICBBbGlnbm1lbnQgPSB0YVJpZ2h0SnVzdGlmeQ0KICAgICAgICBUYWJPcmRlciA9IDE0DQogICAgICAgIFRleHQgPSAnMCcNCiAgICAgICAgVmFsdWUgPSAwDQogICAgICBlbmQNCiAgICAgIG9iamVjdCBXRGF0ZUVkaXRCb3gxOiBUV0RhdGVFZGl0Qm94DQogICAgICAgIExlZnQgPSAxMzYNCiAgICAgICAgSGVpZ2h0ID0gMjMNCiAgICAgICAgVG9wID0gMzkyDQogICAgICAgIFdpZHRoID0gMTQ0DQogICAgICAgIFRhYk9yZGVyID0gMTUNCiAgICAgICAgVGV4dCA9ICczMC4xMi4xODk5Jw0KICAgICAgZW5kDQogICAgICBvYmplY3QgV1RpbWVFZGl0Qm94MTogVFdUaW1lRWRpdEJveA0KICAgICAgICBMZWZ0ID0gMTM2DQogICAgICAgIEhlaWdodCA9IDIzDQogICAgICAgIFRvcCA9IDQzMQ0KICAgICAgICBXaWR0aCA9IDE0NA0KICAgICAgICBUYWJPcmRlciA9IDE2DQogICAgICAgIFRleHQgPSAnMDowMCcNCiAgICAgIGVuZA0KICAgICAgb2JqZWN0IFdGaWxlQnV0dG9uMTogVFdGaWxlQnV0dG9uDQogICAgICAgIExlZnQgPSAxNg0KICAgICAgICBIZWlnaHQgPSAyNQ0KICAgICAgICBUb3AgPSA0NzINCiAgICAgICAgV2lkdGggPSA3NQ0KICAgICAgICBDYXB0aW9uID0gJ1dGaWxlQnV0dG9uMScNCiAgICAgICAgVGFiT3JkZXIgPSAxNw0KICAgICAgZW5kDQogICAgICBvYmplY3QgV0xhYmVsMzogVFdMYWJlbA0KICAgICAgICBMZWZ0ID0gNjA4DQogICAgICAgIEhlaWdodCA9IDI1DQogICAgICAgIFRvcCA9IDQwDQogICAgICAgIFdpZHRoID0gNzMNCiAgICAgICAgQ2FwdGlvbiA9ICdXTGFiZWwzJw0KICAgICAgICBGb250LkNvbG9yID0gY2xBcXVhDQogICAgICAgIEZvbnQuSGVpZ2h0ID0gLTE5DQogICAgICAgIFBhcmVudEZvbnQgPSBGYWxzZQ0KICAgICAgZW5kDQogICAgICBvYmplY3QgV0J1dHRvbjk6IFRXQnV0dG9uDQogICAgICAgIExlZnQgPSA2NjQNCiAgICAgICAgSGVpZ2h0ID0gMjUNCiAgICAgICAgVG9wID0gMjMyDQogICAgICAgIFdpZHRoID0gNzUNCiAgICAgICAgQ2FwdGlvbiA9ICdXQnV0dG9uOScNCiAgICAgICAgVGFiT3JkZXIgPSAxOA0KICAgICAgZW5kDQogICAgZW5kDQogICAgb2JqZWN0IFdTcGxpdHRlcjE6IFRXU3BsaXR0ZXINCiAgICAgIEN1cnNvciA9IGNySFNwbGl0DQogICAgICBMZWZ0ID0gODE1DQogICAgICBIZWlnaHQgPSA1MTANCiAgICAgIFRvcCA9IDANCiAgICAgIFdpZHRoID0gNQ0KICAgICAgQWxpZ24gPSBhbFJpZ2h0DQogICAgICBDb2xvciA9IDE2NzQ0NTc2DQogICAgZW5kDQogIGVuZA0KICBvYmplY3QgV1BvcHVwTWVudTE6IFRXUG9wdXBNZW51DQogICAgQXV0b1BvcHVwID0gRmFsc2UNCiAgICBMZWZ0ID0gMTUyDQogICAgVG9wID0gODANCiAgICBvYmplY3QgTWVudUl0ZW0xOiBUTWVudUl0ZW0NCiAgICAgIENhcHRpb24gPSAnTWVudUl0ZW0xJw0KICAgICAgb2JqZWN0IE1lbnVJdGVtNTogVE1lbnVJdGVtDQogICAgICAgIENhcHRpb24gPSAnTWVudUl0ZW01Jw0KICAgICAgZW5kDQogICAgICBvYmplY3QgTWVudUl0ZW02OiBUTWVudUl0ZW0NCiAgICAgICAgQ2FwdGlvbiA9ICdNZW51SXRlbTYnDQogICAgICBlbmQNCiAgICBlbmQNCiAgICBvYmplY3QgTWVudUl0ZW0yOiBUTWVudUl0ZW0NCiAgICAgIENhcHRpb24gPSAnTWVzc2FnZSBESWFsb2cnDQogICAgICBPbkNsaWNrID0gTWVudUl0ZW0yQ2xpY2sNCiAgICBlbmQNCiAgICBvYmplY3QgTWVudUl0ZW0zOiBUTWVudUl0ZW0NCiAgICAgIENhcHRpb24gPSAnTWVudUl0ZW0zJw0KICAgIGVuZA0KICAgIG9iamVjdCBNZW51SXRlbTQ6IFRNZW51SXRlbQ0KICAgICAgQ2FwdGlvbiA9ICdNZW51SXRlbTQnDQogICAgZW5kDQogIGVuZA0KICBvYmplY3QgV1RpbWVyMTogVFdUaW1lcg0KICAgIE9uVGltZXIgPSBXVGltZXIxVGltZXINCiAgICBMZWZ0ID0gNjI0DQogICAgVG9wID0gMTc2DQogIGVuZA0KZW5kDQo="});
rtl.addResource({name: "unit1", unit: "unit1", format: "application/octet-stream", encoding: "base64", data: "b2JqZWN0IFdGb3JtMjogVFdGb3JtMg0KICBMZWZ0ID0gMzQyDQogIEhlaWdodCA9IDM3Mw0KICBUb3AgPSAyNTANCiAgV2lkdGggPSA0NDMNCiAgQWxwaGFCbGVuZCA9IEZhbHNlDQogIEFscGhhQmxlbmRWYWx1ZSA9IDI1NQ0KICBDYXB0aW9uID0gJ1dGb3JtMicNCiAgQ2xpZW50SGVpZ2h0ID0gMzczDQogIENsaWVudFdpZHRoID0gNDQzDQogIG9iamVjdCBXQnV0dG9uMTogVFdCdXR0b24NCiAgICBMZWZ0ID0gMTA4DQogICAgSGVpZ2h0ID0gMjUNCiAgICBUb3AgPSAxNDANCiAgICBXaWR0aCA9IDc1DQogICAgQ2FwdGlvbiA9ICdCQUNLJw0KICAgIFRhYk9yZGVyID0gMA0KICAgIE9uQ2xpY2sgPSBXQnV0dG9uMUNsaWNrDQogIGVuZA0KZW5kDQo="});
rtl.addResource({name: "demo", unit: "demo", format: "application/octet-stream", encoding: "base64", data: "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"});
rtl.module("System",[],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.LineEnding = "\n";
  this.sLineBreak = this.LineEnding;
  this.MaxLongint = 0x7fffffff;
  this.Maxint = 2147483647;
  this.$rtti.$inherited("TDateTime",rtl.double,{});
  this.$rtti.$inherited("TTime",this.$rtti["TDateTime"],{});
  this.$rtti.$inherited("TDate",this.$rtti["TDateTime"],{});
  this.TTextLineBreakStyle = {"0": "tlbsLF", tlbsLF: 0, "1": "tlbsCRLF", tlbsCRLF: 1, "2": "tlbsCR", tlbsCR: 2};
  rtl.recNewT(this,"TGuid",function () {
    this.D1 = 0;
    this.D2 = 0;
    this.D3 = 0;
    this.$new = function () {
      var r = Object.create(this);
      r.D4 = rtl.arraySetLength(null,0,8);
      return r;
    };
    this.$eq = function (b) {
      return (this.D1 === b.D1) && (this.D2 === b.D2) && (this.D3 === b.D3) && rtl.arrayEq(this.D4,b.D4);
    };
    this.$assign = function (s) {
      this.D1 = s.D1;
      this.D2 = s.D2;
      this.D3 = s.D3;
      this.D4 = s.D4.slice(0);
      return this;
    };
  });
  rtl.recNewT(this,"TMethod",function () {
    this.Code = null;
    this.Data = null;
    this.$eq = function (b) {
      return (this.Code === b.Code) && (this.Data === b.Data);
    };
    this.$assign = function (s) {
      this.Code = s.Code;
      this.Data = s.Data;
      return this;
    };
  });
  this.$rtti.$Class("TObject");
  this.$rtti.$ClassRef("TClass",{instancetype: this.$rtti["TObject"]});
  rtl.createClass(this,"TObject",null,function () {
    this.$init = function () {
    };
    this.$final = function () {
    };
    this.Create = function () {
      return this;
    };
    this.Destroy = function () {
    };
    this.Free = function () {
      this.$destroy("Destroy");
    };
    this.ClassType = function () {
      return this;
    };
    this.InheritsFrom = function (aClass) {
      return (aClass!=null) && ((this==aClass) || aClass.isPrototypeOf(this));
    };
    this.MethodAddress = function (aName) {
      var Result = null;
      Result = null;
      if (aName === "") return Result;
      var i = 0;
        var TI = this.$rtti;
        var N = "";
        var MN = "";
        N = aName.toLowerCase();
        while ((MN === "") && (TI != null)) {
          i = 0;
          while ((MN === "") && (i < TI.methods.length)) {
            if (TI.getMethod(i).name.toLowerCase() === N) MN = TI.getMethod(i).name;
            i += 1;
          };
          if (MN === "") TI = TI.ancestor;
        };
        if (MN !== "") Result = this[MN];
      //  return Result;
      return Result;
    };
    this.FieldAddress = function (aName) {
      var Result = null;
      Result = null;
      if (aName === "") return Result;
      var aClass = this.$class;
      var ClassTI = null;
      var myName = aName.toLowerCase();
      var MemberTI = null;
      while (aClass !== null) {
        ClassTI = aClass.$rtti;
        for (var i = 0, $end2 = ClassTI.fields.length - 1; i <= $end2; i++) {
          MemberTI = ClassTI.getField(i);
          if (MemberTI.name.toLowerCase() === myName) {
             return MemberTI;
          };
        };
        aClass = aClass.$ancestor ? aClass.$ancestor : null;
      };
      return Result;
    };
    this.AfterConstruction = function () {
    };
    this.BeforeDestruction = function () {
    };
    this.GetInterface = function (iid, obj) {
      var Result = false;
      var i = iid.$intf;
      if (i){
        // iid is the private TGuid of an interface
        i = rtl.getIntfG(this,i.$guid,2);
        if (i){
          obj.set(i);
          return true;
        }
      };
      Result = this.GetInterfaceByStr(rtl.guidrToStr(iid),obj);
      return Result;
    };
    this.GetInterfaceByStr = function (iidstr, obj) {
      var Result = false;
      Result = false;
      if (!$mod.IObjectInstance["$str"]) $mod.IObjectInstance["$str"] = rtl.guidrToStr($mod.IObjectInstance);
      if (iidstr == $mod.IObjectInstance["$str"]) {
        obj.set(this);
        return true;
      };
      var i = rtl.getIntfG(this,iidstr,2);
      obj.set(i);
      Result=(i!==null);
      return Result;
    };
  });
  this.S_OK = 0;
  this.E_NOINTERFACE = -2147467262;
  rtl.createInterface(this,"IUnknown","{00000000-0000-0000-C000-000000000046}",["QueryInterface","_AddRef","_Release"],null,function () {
    this.$kind = "com";
  });
  this.IObjectInstance = this.TGuid.$clone({D1: 0xD91C9AF4, D2: 0x3C93, D3: 0x420F, D4: [0xA3,0x03,0xBF,0x5B,0xA8,0x2B,0xFD,0x23]});
  this.TTypeKind = {"0": "tkUnknown", tkUnknown: 0, "1": "tkInteger", tkInteger: 1, "2": "tkChar", tkChar: 2, "3": "tkString", tkString: 3, "4": "tkEnumeration", tkEnumeration: 4, "5": "tkSet", tkSet: 5, "6": "tkDouble", tkDouble: 6, "7": "tkBool", tkBool: 7, "8": "tkProcVar", tkProcVar: 8, "9": "tkMethod", tkMethod: 9, "10": "tkArray", tkArray: 10, "11": "tkDynArray", tkDynArray: 11, "12": "tkRecord", tkRecord: 12, "13": "tkClass", tkClass: 13, "14": "tkClassRef", tkClassRef: 14, "15": "tkPointer", tkPointer: 15, "16": "tkJSValue", tkJSValue: 16, "17": "tkRefToProcVar", tkRefToProcVar: 17, "18": "tkInterface", tkInterface: 18, "19": "tkHelper", tkHelper: 19, "20": "tkExtClass", tkExtClass: 20};
  this.tkFloat = 6;
  this.vtInteger = 0;
  this.vtExtended = 3;
  this.vtWideChar = 9;
  this.vtCurrency = 12;
  this.vtUnicodeString = 18;
  this.vtNativeInt = 19;
  rtl.recNewT(this,"TVarRec",function () {
    this.VType = 0;
    this.VJSValue = undefined;
    this.$eq = function (b) {
      return (this.VType === b.VType) && (this.VJSValue === b.VJSValue) && (this.VJSValue === b.VJSValue) && (this.VJSValue === b.VJSValue) && (this.VJSValue === b.VJSValue) && (this.VJSValue === b.VJSValue) && (this.VJSValue === b.VJSValue) && (this.VJSValue === b.VJSValue);
    };
    this.$assign = function (s) {
      this.VType = s.VType;
      this.VJSValue = s.VJSValue;
      this.VJSValue = s.VJSValue;
      this.VJSValue = s.VJSValue;
      this.VJSValue = s.VJSValue;
      this.VJSValue = s.VJSValue;
      this.VJSValue = s.VJSValue;
      this.VJSValue = s.VJSValue;
      return this;
    };
  });
  this.VarRecs = function () {
    var Result = [];
    var i = 0;
    var v = null;
    Result = [];
    while (i < arguments.length) {
      v = $mod.TVarRec.$new();
      v.VType = rtl.trunc(arguments[i]);
      i += 1;
      v.VJSValue = arguments[i];
      i += 1;
      Result.push($mod.TVarRec.$clone(v));
    };
    return Result;
  };
  this.Trunc = function (A) {
    if (!Math.trunc) {
      Math.trunc = function(v) {
        v = +v;
        if (!isFinite(v)) return v;
        return (v - v % 1) || (v < 0 ? -0 : v === 0 ? v : 0);
      };
    }
    $mod.Trunc = Math.trunc;
    return Math.trunc(A);
  };
  this.DefaultTextLineBreakStyle = 0;
  this.Int = function (A) {
    var Result = 0.0;
    Result = $mod.Trunc(A);
    return Result;
  };
  this.Copy = function (S, Index, Size) {
    if (Index<1) Index = 1;
    return (Size>0) ? S.substring(Index-1,Index+Size-1) : "";
  };
  this.Copy$1 = function (S, Index) {
    if (Index<1) Index = 1;
    return S.substr(Index-1);
  };
  this.Delete = function (S, Index, Size) {
    var h = "";
    if ((Index < 1) || (Index > S.get().length) || (Size <= 0)) return;
    h = S.get();
    S.set($mod.Copy(h,1,Index - 1) + $mod.Copy$1(h,Index + Size));
  };
  this.Pos = function (Search, InString) {
    return InString.indexOf(Search)+1;
  };
  this.Insert = function (Insertion, Target, Index) {
    var t = "";
    if (Insertion === "") return;
    t = Target.get();
    if (Index < 1) {
      Target.set(Insertion + t)}
     else if (Index > t.length) {
      Target.set(t + Insertion)}
     else Target.set($mod.Copy(t,1,Index - 1) + Insertion + $mod.Copy(t,Index,t.length));
  };
  this.upcase = function (c) {
    return c.toUpperCase();
  };
  this.val = function (S, NI, Code) {
    NI.set($impl.valint(S,-9007199254740991,9007199254740991,Code));
  };
  this.val$6 = function (S, I, Code) {
    I.set($impl.valint(S,-2147483648,2147483647,Code));
  };
  this.val$8 = function (S, d, Code) {
    var x = 0.0;
    if (S === "") {
      Code.set(1);
      return;
    };
    x = Number(S);
    if (isNaN(x)) {
      Code.set(1)}
     else {
      Code.set(0);
      d.set(x);
    };
  };
  this.StringOfChar = function (c, l) {
    var Result = "";
    var i = 0;
    if ((l>0) && c.repeat) return c.repeat(l);
    Result = "";
    for (var $l = 1, $end = l; $l <= $end; $l++) {
      i = $l;
      Result = Result + c;
    };
    return Result;
  };
  this.Writeln = function () {
    var i = 0;
    var l = 0;
    var s = "";
    l = arguments.length - 1;
    if ($impl.WriteCallBack != null) {
      for (var $l = 0, $end = l; $l <= $end; $l++) {
        i = $l;
        $impl.WriteCallBack(arguments[i],i === l);
      };
    } else {
      s = $impl.WriteBuf;
      for (var $l1 = 0, $end1 = l; $l1 <= $end1; $l1++) {
        i = $l1;
        s = s + ("" + arguments[i]);
      };
      console.log(s);
      $impl.WriteBuf = "";
    };
  };
  this.Assigned = function (V) {
    return (V!=undefined) && (V!=null) && (!rtl.isArray(V) || (V.length > 0));
  };
  $mod.$implcode = function () {
    $impl.WriteBuf = "";
    $impl.WriteCallBack = null;
    $impl.valint = function (S, MinVal, MaxVal, Code) {
      var Result = 0;
      var x = 0.0;
      if (S === "") {
        Code.set(1);
        return Result;
      };
      x = Number(S);
      if (isNaN(x)) {
        var $tmp = $mod.Copy(S,1,1);
        if ($tmp === "$") {
          x = Number("0x" + $mod.Copy$1(S,2))}
         else if ($tmp === "&") {
          x = Number("0o" + $mod.Copy$1(S,2))}
         else if ($tmp === "%") {
          x = Number("0b" + $mod.Copy$1(S,2))}
         else {
          Code.set(1);
          return Result;
        };
      };
      if (isNaN(x) || (x !== $mod.Int(x))) {
        Code.set(1)}
       else if ((x < MinVal) || (x > MaxVal)) {
        Code.set(2)}
       else {
        Result = $mod.Trunc(x);
        Code.set(0);
      };
      return Result;
    };
  };
  $mod.$init = function () {
    rtl.exitcode = 0;
  };
},[]);
rtl.module("RTLConsts",["System"],function () {
  "use strict";
  var $mod = this;
  $mod.$resourcestrings = {SArgumentMissing: {org: 'Missing argument in format "%s"'}, SInvalidFormat: {org: 'Invalid format specifier : "%s"'}, SInvalidArgIndex: {org: 'Invalid argument index in format: "%s"'}, SListCapacityError: {org: "List capacity (%s) exceeded."}, SListCountError: {org: "List count (%s) out of bounds."}, SListIndexError: {org: "List index (%s) out of bounds"}, SSortedListError: {org: "Operation not allowed on sorted list"}, SDuplicateString: {org: "String list does not allow duplicates"}, SErrFindNeedsSortedList: {org: "Cannot use find on unsorted list"}, SInvalidName: {org: 'Invalid component name: "%s"'}, SDuplicateName: {org: 'Duplicate component name: "%s"'}, SErrInvalidDate: {org: 'Invalid date: "%s"'}, SErrInvalidTimeFormat: {org: 'Invalid time format: "%s"'}, SInvalidDateFormat: {org: 'Invalid date format: "%s"'}, SCantReadPropertyS: {org: 'Cannot read property "%s"'}, SCantWritePropertyS: {org: 'Cannot write property "%s"'}, SIndexedPropertyNeedsParams: {org: 'Indexed property "%s" needs parameters'}, SErrInvalidInteger: {org: 'Invalid integer value: "%s"'}, SEmptyStreamIllegalReader: {org: "Illegal Nil stream for TReader constructor"}, SInvalidPropertyValue: {org: "Invalid value for property"}, SInvalidImage: {org: "Invalid stream format"}, SUnknownProperty: {org: 'Unknown property: "%s"'}, SUnknownPropertyType: {org: "Unknown property type %s"}, SAncestorNotFound: {org: 'Ancestor class for "%s" not found.'}, SUnsupportedPropertyVariantType: {org: "Unsupported property variant type %d"}, SPropertyException: {org: "Error reading %s%s%s: %s"}, SInvalidPropertyPath: {org: "Invalid property path"}, SReadOnlyProperty: {org: "Property is read-only"}, SClassNotFound: {org: 'Class "%s" not found'}, SParserExpected: {org: "Wrong token type: %s expected"}, SParserInvalidFloat: {org: "Invalid floating point number: %s"}, SParserInvalidInteger: {org: "Invalid integer number: %s"}, SParserUnterminatedString: {org: "Unterminated string"}, SParserWrongTokenType: {org: "Wrong token type: %s expected but %s found"}, SParserWrongTokenSymbol: {org: "Wrong token symbol: %s expected but %s found"}, SParserLocInfo: {org: " (at %d,%d, stream offset %.8x)"}, SParserUnterminatedBinValue: {org: "Unterminated byte value"}, SParserInvalidProperty: {org: "Invalid property"}};
});
rtl.module("Types",["System"],function () {
  "use strict";
  var $mod = this;
  this.TDirection = {"0": "FromBeginning", FromBeginning: 0, "1": "FromEnd", FromEnd: 1};
  this.TDuplicates = {"0": "dupIgnore", dupIgnore: 0, "1": "dupAccept", dupAccept: 1, "2": "dupError", dupError: 2};
  rtl.recNewT(this,"TSize",function () {
    this.cx = 0;
    this.cy = 0;
    this.$eq = function (b) {
      return (this.cx === b.cx) && (this.cy === b.cy);
    };
    this.$assign = function (s) {
      this.cx = s.cx;
      this.cy = s.cy;
      return this;
    };
  });
  rtl.recNewT(this,"TPoint",function () {
    this.x = 0;
    this.y = 0;
    this.$eq = function (b) {
      return (this.x === b.x) && (this.y === b.y);
    };
    this.$assign = function (s) {
      this.x = s.x;
      this.y = s.y;
      return this;
    };
    var $r = $mod.$rtti.$Record("TPoint",{});
    $r.addField("x",rtl.longint);
    $r.addField("y",rtl.longint);
  });
  rtl.recNewT(this,"TRect",function () {
    this.Left = 0;
    this.Top = 0;
    this.Right = 0;
    this.Bottom = 0;
    this.$eq = function (b) {
      return (this.Left === b.Left) && (this.Top === b.Top) && (this.Right === b.Right) && (this.Bottom === b.Bottom);
    };
    this.$assign = function (s) {
      this.Left = s.Left;
      this.Top = s.Top;
      this.Right = s.Right;
      this.Bottom = s.Bottom;
      return this;
    };
  });
  this.Rect = function (Left, Top, Right, Bottom) {
    var Result = $mod.TRect.$new();
    Result.Left = Left;
    Result.Top = Top;
    Result.Right = Right;
    Result.Bottom = Bottom;
    return Result;
  };
  this.Point = function (x, y) {
    var Result = $mod.TPoint.$new();
    Result.x = x;
    Result.y = y;
    return Result;
  };
  this.Size = function (AWidth, AHeight) {
    var Result = $mod.TSize.$new();
    Result.cx = AWidth;
    Result.cy = AHeight;
    return Result;
  };
});
rtl.module("JS",["System","Types"],function () {
  "use strict";
  var $mod = this;
  this.isClassInstance = function (v) {
    return (typeof(v)=="object") && (v!=null) && (v.$class == Object.getPrototypeOf(v));
  };
  this.isDefined = function (v) {
    return !(v == undefined);
  };
});
rtl.module("SysUtils",["System","RTLConsts","JS"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.FreeAndNil = function (Obj) {
    var o = null;
    o = Obj.get();
    if (o === null) return;
    Obj.set(null);
    o.$destroy("Destroy");
  };
  this.TEndian = {"0": "Little", Little: 0, "1": "Big", Big: 1};
  rtl.recNewT(this,"TFormatSettings",function () {
    this.CurrencyDecimals = 0;
    this.CurrencyFormat = 0;
    this.CurrencyString = "";
    this.DateSeparator = "";
    this.DecimalSeparator = "";
    this.LongDateFormat = "";
    this.LongTimeFormat = "";
    this.NegCurrFormat = 0;
    this.ShortDateFormat = "";
    this.ShortTimeFormat = "";
    this.ThousandSeparator = "";
    this.TimeAMString = "";
    this.TimePMString = "";
    this.TimeSeparator = "";
    this.TwoDigitYearCenturyWindow = 0;
    this.InitLocaleHandler = null;
    this.$new = function () {
      var r = Object.create(this);
      r.DateTimeToStrFormat = rtl.arraySetLength(null,"",2);
      r.LongDayNames = rtl.arraySetLength(null,"",7);
      r.LongMonthNames = rtl.arraySetLength(null,"",12);
      r.ShortDayNames = rtl.arraySetLength(null,"",7);
      r.ShortMonthNames = rtl.arraySetLength(null,"",12);
      return r;
    };
    this.$eq = function (b) {
      return (this.CurrencyDecimals === b.CurrencyDecimals) && (this.CurrencyFormat === b.CurrencyFormat) && (this.CurrencyString === b.CurrencyString) && (this.DateSeparator === b.DateSeparator) && rtl.arrayEq(this.DateTimeToStrFormat,b.DateTimeToStrFormat) && (this.DecimalSeparator === b.DecimalSeparator) && (this.LongDateFormat === b.LongDateFormat) && rtl.arrayEq(this.LongDayNames,b.LongDayNames) && rtl.arrayEq(this.LongMonthNames,b.LongMonthNames) && (this.LongTimeFormat === b.LongTimeFormat) && (this.NegCurrFormat === b.NegCurrFormat) && (this.ShortDateFormat === b.ShortDateFormat) && rtl.arrayEq(this.ShortDayNames,b.ShortDayNames) && rtl.arrayEq(this.ShortMonthNames,b.ShortMonthNames) && (this.ShortTimeFormat === b.ShortTimeFormat) && (this.ThousandSeparator === b.ThousandSeparator) && (this.TimeAMString === b.TimeAMString) && (this.TimePMString === b.TimePMString) && (this.TimeSeparator === b.TimeSeparator) && (this.TwoDigitYearCenturyWindow === b.TwoDigitYearCenturyWindow);
    };
    this.$assign = function (s) {
      this.CurrencyDecimals = s.CurrencyDecimals;
      this.CurrencyFormat = s.CurrencyFormat;
      this.CurrencyString = s.CurrencyString;
      this.DateSeparator = s.DateSeparator;
      this.DateTimeToStrFormat = s.DateTimeToStrFormat.slice(0);
      this.DecimalSeparator = s.DecimalSeparator;
      this.LongDateFormat = s.LongDateFormat;
      this.LongDayNames = s.LongDayNames.slice(0);
      this.LongMonthNames = s.LongMonthNames.slice(0);
      this.LongTimeFormat = s.LongTimeFormat;
      this.NegCurrFormat = s.NegCurrFormat;
      this.ShortDateFormat = s.ShortDateFormat;
      this.ShortDayNames = s.ShortDayNames.slice(0);
      this.ShortMonthNames = s.ShortMonthNames.slice(0);
      this.ShortTimeFormat = s.ShortTimeFormat;
      this.ThousandSeparator = s.ThousandSeparator;
      this.TimeAMString = s.TimeAMString;
      this.TimePMString = s.TimePMString;
      this.TimeSeparator = s.TimeSeparator;
      this.TwoDigitYearCenturyWindow = s.TwoDigitYearCenturyWindow;
      return this;
    };
    this.GetJSLocale = function () {
      return Intl.DateTimeFormat().resolvedOptions().locale;
    };
    this.Create = function () {
      var Result = $mod.TFormatSettings.$new();
      Result.$assign($mod.TFormatSettings.Create$1($mod.TFormatSettings.GetJSLocale()));
      return Result;
    };
    this.Create$1 = function (ALocale) {
      var Result = $mod.TFormatSettings.$new();
      Result.LongDayNames = $impl.DefaultLongDayNames.slice(0);
      Result.ShortDayNames = $impl.DefaultShortDayNames.slice(0);
      Result.ShortMonthNames = $impl.DefaultShortMonthNames.slice(0);
      Result.LongMonthNames = $impl.DefaultLongMonthNames.slice(0);
      Result.DateTimeToStrFormat[0] = "c";
      Result.DateTimeToStrFormat[1] = "f";
      Result.DateSeparator = "-";
      Result.TimeSeparator = ":";
      Result.ShortDateFormat = "yyyy-mm-dd";
      Result.LongDateFormat = "ddd, yyyy-mm-dd";
      Result.ShortTimeFormat = "hh:nn";
      Result.LongTimeFormat = "hh:nn:ss";
      Result.DecimalSeparator = ".";
      Result.ThousandSeparator = ",";
      Result.TimeAMString = "AM";
      Result.TimePMString = "PM";
      Result.TwoDigitYearCenturyWindow = 50;
      Result.CurrencyFormat = 0;
      Result.NegCurrFormat = 0;
      Result.CurrencyDecimals = 2;
      Result.CurrencyString = "$";
      if ($mod.TFormatSettings.InitLocaleHandler != null) $mod.TFormatSettings.InitLocaleHandler($mod.UpperCase(ALocale),$mod.TFormatSettings.$clone(Result));
      return Result;
    };
  },true);
  rtl.createClass(this,"Exception",pas.System.TObject,function () {
    this.LogMessageOnCreate = false;
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.fMessage = "";
    };
    this.Create$1 = function (Msg) {
      this.fMessage = Msg;
      if (this.LogMessageOnCreate) pas.System.Writeln("Created exception ",this.$classname," with message: ",Msg);
      return this;
    };
    this.CreateFmt = function (Msg, Args) {
      this.Create$1($mod.Format(Msg,Args));
      return this;
    };
  });
  rtl.createClass(this,"EConvertError",this.Exception,function () {
  });
  rtl.createClass(this,"EHeapMemoryError",this.Exception,function () {
  });
  rtl.createClass(this,"EOutOfMemory",this.EHeapMemoryError,function () {
  });
  this.TrimLeft = function (S) {
    return S.replace(/^[\s\uFEFF\xA0\x00-\x1f]+/,'');
  };
  this.UpperCase = function (s) {
    return s.toUpperCase();
  };
  this.LowerCase = function (s) {
    return s.toLowerCase();
  };
  this.CompareStr = function (s1, s2) {
    var l1 = s1.length;
    var l2 = s2.length;
    if (l1<=l2){
      var s = s2.substr(0,l1);
      if (s1<s){ return -1;
      } else if (s1>s){ return 1;
      } else { return l1<l2 ? -1 : 0; };
    } else {
      var s = s1.substr(0,l2);
      if (s<s2){ return -1;
      } else { return 1; };
    };
  };
  this.CompareText = function (s1, s2) {
    var l1 = s1.toLowerCase();
    var l2 = s2.toLowerCase();
    if (l1>l2){ return 1;
    } else if (l1<l2){ return -1;
    } else { return 0; };
  };
  this.SameText = function (s1, s2) {
    return s1.toLowerCase() == s2.toLowerCase();
  };
  this.Format = function (Fmt, Args) {
    var Result = "";
    Result = $mod.Format$1(Fmt,Args,$mod.FormatSettings);
    return Result;
  };
  this.Format$1 = function (Fmt, Args, aSettings) {
    var Result = "";
    var ChPos = 0;
    var OldPos = 0;
    var ArgPos = 0;
    var DoArg = 0;
    var Len = 0;
    var Hs = "";
    var ToAdd = "";
    var Index = 0;
    var Width = 0;
    var Prec = 0;
    var Left = false;
    var Fchar = "";
    var vq = 0;
    function ReadFormat() {
      var Result = "";
      var Value = 0;
      function ReadInteger() {
        var Code = 0;
        var ArgN = 0;
        if (Value !== -1) return;
        OldPos = ChPos;
        while ((ChPos <= Len) && (Fmt.charAt(ChPos - 1) <= "9") && (Fmt.charAt(ChPos - 1) >= "0")) ChPos += 1;
        if (ChPos > Len) $impl.DoFormatError(1,Fmt);
        if (Fmt.charAt(ChPos - 1) === "*") {
          if (Index === 255) {
            ArgN = ArgPos}
           else {
            ArgN = Index;
            Index += 1;
          };
          if ((ChPos > OldPos) || (ArgN > (rtl.length(Args) - 1))) $impl.DoFormatError(1,Fmt);
          ArgPos = ArgN + 1;
          var $tmp = Args[ArgN].VType;
          if ($tmp === 0) {
            Value = Args[ArgN].VJSValue}
           else if ($tmp === 19) {
            Value = Args[ArgN].VJSValue}
           else {
            $impl.DoFormatError(1,Fmt);
          };
          ChPos += 1;
        } else {
          if (OldPos < ChPos) {
            pas.System.val(pas.System.Copy(Fmt,OldPos,ChPos - OldPos),{get: function () {
                return Value;
              }, set: function (v) {
                Value = v;
              }},{get: function () {
                return Code;
              }, set: function (v) {
                Code = v;
              }});
            if (Code > 0) $impl.DoFormatError(1,Fmt);
          } else Value = -1;
        };
      };
      function ReadIndex() {
        if (Fmt.charAt(ChPos - 1) !== ":") {
          ReadInteger()}
         else Value = 0;
        if (Fmt.charAt(ChPos - 1) === ":") {
          if (Value === -1) $impl.DoFormatError(2,Fmt);
          Index = Value;
          Value = -1;
          ChPos += 1;
        };
      };
      function ReadLeft() {
        if (Fmt.charAt(ChPos - 1) === "-") {
          Left = true;
          ChPos += 1;
        } else Left = false;
      };
      function ReadWidth() {
        ReadInteger();
        if (Value !== -1) {
          Width = Value;
          Value = -1;
        };
      };
      function ReadPrec() {
        if (Fmt.charAt(ChPos - 1) === ".") {
          ChPos += 1;
          ReadInteger();
          if (Value === -1) Value = 0;
          Prec = Value;
        };
      };
      Index = 255;
      Width = -1;
      Prec = -1;
      Value = -1;
      ChPos += 1;
      if (Fmt.charAt(ChPos - 1) === "%") {
        Result = "%";
        return Result;
      };
      ReadIndex();
      ReadLeft();
      ReadWidth();
      ReadPrec();
      Result = pas.System.upcase(Fmt.charAt(ChPos - 1));
      return Result;
    };
    function Checkarg(AT, err) {
      var Result = false;
      Result = false;
      if (Index === 255) {
        DoArg = ArgPos}
       else DoArg = Index;
      ArgPos = DoArg + 1;
      if ((DoArg > (rtl.length(Args) - 1)) || (Args[DoArg].VType !== AT)) {
        if (err) $impl.DoFormatError(3,Fmt);
        ArgPos -= 1;
        return Result;
      };
      Result = true;
      return Result;
    };
    Result = "";
    Len = Fmt.length;
    ChPos = 1;
    OldPos = 1;
    ArgPos = 0;
    while (ChPos <= Len) {
      while ((ChPos <= Len) && (Fmt.charAt(ChPos - 1) !== "%")) ChPos += 1;
      if (ChPos > OldPos) Result = Result + pas.System.Copy(Fmt,OldPos,ChPos - OldPos);
      if (ChPos < Len) {
        Fchar = ReadFormat();
        var $tmp = Fchar;
        if ($tmp === "D") {
          if (Checkarg(0,false)) {
            ToAdd = $mod.IntToStr(Args[DoArg].VJSValue)}
           else if (Checkarg(19,true)) ToAdd = $mod.IntToStr(Args[DoArg].VJSValue);
          Width = Math.abs(Width);
          Index = Prec - ToAdd.length;
          if (ToAdd.charAt(0) !== "-") {
            ToAdd = pas.System.StringOfChar("0",Index) + ToAdd}
           else pas.System.Insert(pas.System.StringOfChar("0",Index + 1),{get: function () {
              return ToAdd;
            }, set: function (v) {
              ToAdd = v;
            }},2);
        } else if ($tmp === "U") {
          if (Checkarg(0,false)) {
            ToAdd = $mod.IntToStr(Args[DoArg].VJSValue >>> 0)}
           else if (Checkarg(19,true)) ToAdd = $mod.IntToStr(Args[DoArg].VJSValue);
          Width = Math.abs(Width);
          Index = Prec - ToAdd.length;
          ToAdd = pas.System.StringOfChar("0",Index) + ToAdd;
        } else if ($tmp === "E") {
          if (Checkarg(12,false)) {
            ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue / 10000,2,3,Prec,aSettings)}
           else if (Checkarg(3,true)) ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue,2,3,Prec,aSettings);
        } else if ($tmp === "F") {
          if (Checkarg(12,false)) {
            ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue / 10000,0,9999,Prec,aSettings)}
           else if (Checkarg(3,true)) ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue,0,9999,Prec,aSettings);
        } else if ($tmp === "G") {
          if (Checkarg(12,false)) {
            ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue / 10000,1,Prec,3,aSettings)}
           else if (Checkarg(3,true)) ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue,1,Prec,3,aSettings);
        } else if ($tmp === "N") {
          if (Checkarg(12,false)) {
            ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue / 10000,3,9999,Prec,aSettings)}
           else if (Checkarg(3,true)) ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue,3,9999,Prec,aSettings);
        } else if ($tmp === "M") {
          if (Checkarg(12,false)) {
            ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue / 10000,4,9999,Prec,aSettings)}
           else if (Checkarg(3,true)) ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue,4,9999,Prec,aSettings);
        } else if ($tmp === "S") {
          if (Checkarg(18,false)) {
            Hs = Args[DoArg].VJSValue}
           else if (Checkarg(9,true)) Hs = Args[DoArg].VJSValue;
          Index = Hs.length;
          if ((Prec !== -1) && (Index > Prec)) Index = Prec;
          ToAdd = pas.System.Copy(Hs,1,Index);
        } else if ($tmp === "P") {
          if (Checkarg(0,false)) {
            ToAdd = $mod.IntToHex(Args[DoArg].VJSValue,8)}
           else if (Checkarg(0,true)) ToAdd = $mod.IntToHex(Args[DoArg].VJSValue,16);
        } else if ($tmp === "X") {
          if (Checkarg(0,false)) {
            vq = Args[DoArg].VJSValue;
            Index = 16;
          } else if (Checkarg(19,true)) {
            vq = Args[DoArg].VJSValue;
            Index = 31;
          };
          if (Prec > Index) {
            ToAdd = $mod.IntToHex(vq,Index)}
           else {
            Index = 1;
            while ((rtl.shl(1,Index * 4) <= vq) && (Index < 16)) Index += 1;
            if (Index > Prec) Prec = Index;
            ToAdd = $mod.IntToHex(vq,Prec);
          };
        } else if ($tmp === "%") ToAdd = "%";
        if (Width !== -1) if (ToAdd.length < Width) if (!Left) {
          ToAdd = pas.System.StringOfChar(" ",Width - ToAdd.length) + ToAdd}
         else ToAdd = ToAdd + pas.System.StringOfChar(" ",Width - ToAdd.length);
        Result = Result + ToAdd;
      };
      ChPos += 1;
      OldPos = ChPos;
    };
    return Result;
  };
  var Alpha = rtl.createSet(null,65,90,null,97,122,95);
  var AlphaNum = rtl.unionSet(Alpha,rtl.createSet(null,48,57));
  var Dot = ".";
  this.IsValidIdent = function (Ident, AllowDots, StrictDots) {
    var Result = false;
    var First = false;
    var I = 0;
    var Len = 0;
    Len = Ident.length;
    if (Len < 1) return false;
    First = true;
    Result = false;
    I = 1;
    while (I <= Len) {
      if (First) {
        if (!(Ident.charCodeAt(I - 1) in Alpha)) return Result;
        First = false;
      } else if (AllowDots && (Ident.charAt(I - 1) === Dot)) {
        if (StrictDots) {
          if (I >= Len) return Result;
          First = true;
        };
      } else if (!(Ident.charCodeAt(I - 1) in AlphaNum)) return Result;
      I = I + 1;
    };
    Result = true;
    return Result;
  };
  this.TStringReplaceFlag = {"0": "rfReplaceAll", rfReplaceAll: 0, "1": "rfIgnoreCase", rfIgnoreCase: 1};
  this.StringReplace = function (aOriginal, aSearch, aReplace, Flags) {
    var Result = "";
    var REFlags = "";
    var REString = "";
    REFlags = "";
    if (0 in Flags) REFlags = "g";
    if (1 in Flags) REFlags = REFlags + "i";
    REString = aSearch.replace(new RegExp($impl.RESpecials,"g"),"\\$1");
    Result = aOriginal.replace(new RegExp(REString,REFlags),aReplace);
    return Result;
  };
  this.IntToStr = function (Value) {
    var Result = "";
    Result = "" + Value;
    return Result;
  };
  this.TryStrToInt$1 = function (S, res) {
    var Result = false;
    Result = $impl.IntTryStrToInt(S,res,$mod.FormatSettings.DecimalSeparator);
    return Result;
  };
  this.StrToIntDef = function (S, aDef) {
    var Result = 0;
    var R = 0;
    if ($mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }})) {
      Result = R}
     else Result = aDef;
    return Result;
  };
  this.TryStrToInt64 = function (S, res) {
    var Result = false;
    var R = 0;
    Result = $mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }});
    if (Result) res.set(R);
    return Result;
  };
  this.StrToQWord = function (S) {
    var Result = 0;
    var N = 0;
    if (!$mod.TryStrToInt$1(S,{get: function () {
        return N;
      }, set: function (v) {
        N = v;
      }}) || (N < 0)) throw $mod.EConvertError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SErrInvalidInteger"),pas.System.VarRecs(18,S)]);
    Result = N;
    return Result;
  };
  this.IntToHex = function (Value, Digits) {
    var Result = "";
    Result = "";
    if (Value < 0) if (Value<0) Value = 0xFFFFFFFF + Value + 1;
    Result=Value.toString(16);
    Result = $mod.UpperCase(Result);
    while (Result.length < Digits) Result = "0" + Result;
    return Result;
  };
  this.TFloatFormat = {"0": "ffFixed", ffFixed: 0, "1": "ffGeneral", ffGeneral: 1, "2": "ffExponent", ffExponent: 2, "3": "ffNumber", ffNumber: 3, "4": "ffCurrency", ffCurrency: 4};
  this.FloatToStr = function (Value) {
    var Result = "";
    Result = $mod.FloatToStr$1(Value,$mod.FormatSettings);
    return Result;
  };
  this.FloatToStr$1 = function (Value, aSettings) {
    var Result = "";
    Result = $mod.FloatToStrF$1(Value,1,15,0,aSettings);
    return Result;
  };
  this.FloatToStrF = function (Value, format, Precision, Digits) {
    var Result = "";
    Result = $mod.FloatToStrF$1(Value,format,Precision,Digits,$mod.FormatSettings);
    return Result;
  };
  this.FloatToStrF$1 = function (Value, format, Precision, Digits, aSettings) {
    var Result = "";
    var TS = "";
    var DS = "";
    DS = aSettings.DecimalSeparator;
    TS = aSettings.ThousandSeparator;
    var $tmp = format;
    if ($tmp === 1) {
      Result = $impl.FormatGeneralFloat(Value,Precision,DS)}
     else if ($tmp === 2) {
      Result = $impl.FormatExponentFloat(Value,Precision,Digits,DS)}
     else if ($tmp === 0) {
      Result = $impl.FormatFixedFloat(Value,Digits,DS)}
     else if ($tmp === 3) {
      Result = $impl.FormatNumberFloat(Value,Digits,DS,TS)}
     else if ($tmp === 4) Result = $impl.FormatNumberCurrency(Value * 10000,Digits,aSettings);
    if ((format !== 4) && (Result.length > 1) && (Result.charAt(0) === "-")) $impl.RemoveLeadingNegativeSign({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},DS,TS);
    return Result;
  };
  this.TryStrToFloat$3 = function (S, res, aSettings) {
    var Result = false;
    var J = undefined;
    var N = "";
    N = S;
    if (aSettings.ThousandSeparator !== "") N = $mod.StringReplace(N,aSettings.ThousandSeparator,"",rtl.createSet(0));
    if (aSettings.DecimalSeparator !== ".") N = $mod.StringReplace(N,aSettings.DecimalSeparator,".",{});
    J = parseFloat(N);
    Result = !isNaN(J);
    if (Result) res.set(rtl.getNumber(J));
    return Result;
  };
  this.StrToFloatDef = function (S, aDef) {
    var Result = 0.0;
    if (!$mod.TryStrToFloat$3(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},$mod.FormatSettings)) Result = aDef;
    return Result;
  };
  this.OnShowException = null;
  this.SetOnUnCaughtExceptionHandler = function (aValue) {
    var Result = null;
    Result = $impl.OnPascalException;
    $impl.OnPascalException = aValue;
    $mod.HookUncaughtExceptions();
    return Result;
  };
  this.HookUncaughtExceptions = function () {
    rtl.onUncaughtException = $impl.RTLExceptionHook;
    rtl.showUncaughtExceptions = true;
  };
  this.ShowException = function (ExceptObject, ExceptAddr) {
    var S = "";
    S = rtl.getResStr($mod,"SApplicationException") + ExceptObject.$classname;
    if ($mod.Exception.isPrototypeOf(ExceptObject)) S = S + " : " + ExceptObject.fMessage;
    $impl.DoShowException(S);
    if (ExceptAddr === null) ;
  };
  rtl.recNewT(this,"TTimeStamp",function () {
    this.Time = 0;
    this.Date = 0;
    this.$eq = function (b) {
      return (this.Time === b.Time) && (this.Date === b.Date);
    };
    this.$assign = function (s) {
      this.Time = s.Time;
      this.Date = s.Date;
      return this;
    };
  });
  this.TimeSeparator = "";
  this.DateSeparator = "";
  this.ShortDateFormat = "";
  this.LongDateFormat = "";
  this.ShortTimeFormat = "";
  this.LongTimeFormat = "";
  this.DecimalSeparator = "";
  this.ThousandSeparator = "";
  this.TimeAMString = "";
  this.TimePMString = "";
  this.HoursPerDay = 24;
  this.MinsPerHour = 60;
  this.SecsPerMin = 60;
  this.MSecsPerSec = 1000;
  this.MinsPerDay = 24 * 60;
  this.SecsPerDay = 1440 * 60;
  this.MSecsPerDay = 86400 * 1000;
  this.MaxDateTime = 2958465.99999999;
  this.DateDelta = 693594;
  this.MonthDays$a$clone = function (a) {
    var b = [];
    b.length = 2;
    for (var c = 0; c < 2; c++) b[c] = a[c].slice(0);
    return b;
  };
  this.MonthDays = [[31,28,31,30,31,30,31,31,30,31,30,31],[31,29,31,30,31,30,31,31,30,31,30,31]];
  this.ShortMonthNames = rtl.arraySetLength(null,"",12);
  this.LongMonthNames = rtl.arraySetLength(null,"",12);
  this.ShortDayNames = rtl.arraySetLength(null,"",7);
  this.LongDayNames = rtl.arraySetLength(null,"",7);
  this.FormatSettings = this.TFormatSettings.$new();
  this.TwoDigitYearCenturyWindow = 50;
  this.JSDateToDateTime = function (aDate) {
    var Result = 0.0;
    Result = $mod.EncodeDate(aDate.getFullYear(),aDate.getMonth() + 1,aDate.getDate()) + $mod.EncodeTime(aDate.getHours(),aDate.getMinutes(),aDate.getSeconds(),aDate.getMilliseconds());
    return Result;
  };
  this.DateTimeToTimeStamp = function (DateTime) {
    var Result = $mod.TTimeStamp.$new();
    var D = 0.0;
    D = DateTime * 86400000;
    if (D < 0) {
      D = D - 0.5}
     else D = D + 0.5;
    Result.Time = pas.System.Trunc(Math.abs(pas.System.Trunc(D)) % 86400000);
    Result.Date = 693594 + rtl.trunc(pas.System.Trunc(D) / 86400000);
    return Result;
  };
  this.TryEncodeDate = function (Year, Month, Day, date) {
    var Result = false;
    var c = 0;
    var ya = 0;
    Result = (Year > 0) && (Year < 10000) && (Month >= 1) && (Month <= 12) && (Day > 0) && (Day <= $mod.MonthDays[+$mod.IsLeapYear(Year)][Month - 1]);
    if (Result) {
      if (Month > 2) {
        Month -= 3}
       else {
        Month += 9;
        Year -= 1;
      };
      c = rtl.trunc(Year / 100);
      ya = Year - (100 * c);
      date.set(((146097 * c) >>> 2) + ((1461 * ya) >>> 2) + rtl.trunc(((153 * Month) + 2) / 5) + Day);
      date.set(date.get() - 693900);
    };
    return Result;
  };
  this.TryEncodeTime = function (Hour, Min, Sec, MSec, Time) {
    var Result = false;
    Result = (Hour < 24) && (Min < 60) && (Sec < 60) && (MSec < 1000);
    if (Result) Time.set(((Hour * 3600000) + (Min * 60000) + (Sec * 1000) + MSec) / 86400000);
    return Result;
  };
  this.EncodeDate = function (Year, Month, Day) {
    var Result = 0.0;
    if (!$mod.TryEncodeDate(Year,Month,Day,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",["%s-%s-%s is not a valid date specification",pas.System.VarRecs(18,$mod.IntToStr(Year),18,$mod.IntToStr(Month),18,$mod.IntToStr(Day))]);
    return Result;
  };
  this.EncodeTime = function (Hour, Minute, Second, MilliSecond) {
    var Result = 0.0;
    if (!$mod.TryEncodeTime(Hour,Minute,Second,MilliSecond,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",["%s:%s:%s.%s is not a valid time specification",pas.System.VarRecs(18,$mod.IntToStr(Hour),18,$mod.IntToStr(Minute),18,$mod.IntToStr(Second),18,$mod.IntToStr(MilliSecond))]);
    return Result;
  };
  this.DecodeDate = function (date, Year, Month, Day) {
    var ly = 0;
    var ld = 0;
    var lm = 0;
    var j = 0;
    if (date <= -693594) {
      Year.set(0);
      Month.set(0);
      Day.set(0);
    } else {
      if (date > 0) {
        date = date + (1 / (86400000 * 2))}
       else date = date - (1 / (86400000 * 2));
      if (date > $mod.MaxDateTime) date = $mod.MaxDateTime;
      j = rtl.shl(pas.System.Trunc(date) + 693900,2) - 1;
      ly = rtl.trunc(j / 146097);
      j = j - (146097 * ly);
      ld = rtl.lw(j >>> 2);
      j = rtl.trunc((rtl.lw(ld << 2) + 3) / 1461);
      ld = rtl.lw(((rtl.lw(ld << 2) + 7) - (1461 * j)) >>> 2);
      lm = rtl.trunc(((5 * ld) - 3) / 153);
      ld = rtl.trunc((((5 * ld) + 2) - (153 * lm)) / 5);
      ly = (100 * ly) + j;
      if (lm < 10) {
        lm += 3}
       else {
        lm -= 9;
        ly += 1;
      };
      Year.set(ly);
      Month.set(lm);
      Day.set(ld);
    };
  };
  this.DecodeTime = function (Time, Hour, Minute, Second, MilliSecond) {
    var l = 0;
    l = $mod.DateTimeToTimeStamp(Time).Time;
    Hour.set(rtl.trunc(l / 3600000));
    l = l % 3600000;
    Minute.set(rtl.trunc(l / 60000));
    l = l % 60000;
    Second.set(rtl.trunc(l / 1000));
    l = l % 1000;
    MilliSecond.set(l);
  };
  this.DecodeDateFully = function (DateTime, Year, Month, Day, DOW) {
    var Result = false;
    $mod.DecodeDate(DateTime,Year,Month,Day);
    DOW.set($mod.DayOfWeek(DateTime));
    Result = $mod.IsLeapYear(Year.get());
    return Result;
  };
  this.Date = function () {
    var Result = 0.0;
    Result = pas.System.Trunc($mod.Now());
    return Result;
  };
  this.Now = function () {
    var Result = 0.0;
    Result = $mod.JSDateToDateTime(new Date());
    return Result;
  };
  this.DayOfWeek = function (DateTime) {
    var Result = 0;
    Result = 1 + ((pas.System.Trunc(DateTime) - 1) % 7);
    if (Result <= 0) Result += 7;
    return Result;
  };
  this.IsLeapYear = function (Year) {
    var Result = false;
    Result = ((Year % 4) === 0) && (((Year % 100) !== 0) || ((Year % 400) === 0));
    return Result;
  };
  this.DateToStr = function (date) {
    var Result = "";
    Result = $mod.DateToStr$1(date,$mod.FormatSettings);
    return Result;
  };
  this.DateToStr$1 = function (date, aSettings) {
    var Result = "";
    Result = $mod.FormatDateTime$1("ddddd",date,aSettings);
    return Result;
  };
  this.TryStrToDate$2 = function (S, Value, separator) {
    var Result = false;
    Result = $mod.TryStrToDate$3(S,Value,$mod.FormatSettings.ShortDateFormat,separator);
    return Result;
  };
  this.TryStrToDate$3 = function (S, Value, useformat, separator) {
    var Result = false;
    var Msg = "";
    Result = S.length !== 0;
    if (Result) {
      Value.set($impl.IntStrToDate({get: function () {
          return Msg;
        }, set: function (v) {
          Msg = v;
        }},S,useformat,separator));
      Result = Msg === "";
    };
    return Result;
  };
  this.StrToDateDef = function (S, Defvalue) {
    var Result = 0.0;
    Result = $mod.StrToDateDef$1(S,Defvalue,"\x00");
    return Result;
  };
  this.StrToDateDef$1 = function (S, Defvalue, separator) {
    var Result = 0.0;
    if (!$mod.TryStrToDate$2(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},separator)) Result = Defvalue;
    return Result;
  };
  this.TimeToStr = function (Time) {
    var Result = "";
    Result = $mod.TimeToStr$1(Time,$mod.FormatSettings);
    return Result;
  };
  this.TimeToStr$1 = function (Time, aSettings) {
    var Result = "";
    Result = $mod.FormatDateTime$1("tt",Time,aSettings);
    return Result;
  };
  this.TryStrToTime$1 = function (S, Value, aSettings) {
    var Result = false;
    var Msg = "";
    Result = S.length !== 0;
    if (Result) {
      Value.set($impl.IntStrToTime({get: function () {
          return Msg;
        }, set: function (v) {
          Msg = v;
        }},S,S.length,aSettings));
      Result = Msg === "";
    };
    return Result;
  };
  this.TryStrToTime$2 = function (S, Value, separator) {
    var Result = false;
    var Fmt = $mod.TFormatSettings.$new();
    Fmt.$assign($mod.TFormatSettings.Create());
    Fmt.TimeSeparator = separator;
    Result = $mod.TryStrToTime$1(S,Value,$mod.TFormatSettings.$clone(Fmt));
    return Result;
  };
  this.StrToTimeDef$1 = function (S, Defvalue, separator) {
    var Result = 0.0;
    if (!$mod.TryStrToTime$2(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},separator)) Result = Defvalue;
    return Result;
  };
  this.DateTimeToStr = function (DateTime, ForceTimeIfZero) {
    var Result = "";
    Result = $mod.DateTimeToStr$1(DateTime,$mod.FormatSettings,ForceTimeIfZero);
    return Result;
  };
  this.DateTimeToStr$1 = function (DateTime, aSettings, ForceTimeIfZero) {
    var Result = "";
    Result = $mod.FormatDateTime$1($impl.DateTimeToStrFormat[+ForceTimeIfZero],DateTime,aSettings);
    return Result;
  };
  this.FormatDateTime = function (FormatStr, DateTime) {
    var Result = "";
    Result = $mod.FormatDateTime$1(FormatStr,DateTime,$mod.FormatSettings);
    return Result;
  };
  this.FormatDateTime$1 = function (FormatStr, DateTime, aSettings) {
    var Result = "";
    function StoreStr(APos, Len) {
      Result = Result + pas.System.Copy(FormatStr,APos,Len);
    };
    function StoreString(AStr) {
      Result = Result + AStr;
    };
    function StoreInt(Value, Digits) {
      var S = "";
      S = $mod.IntToStr(Value);
      while (S.length < Digits) S = "0" + S;
      StoreString(S);
    };
    var Year = 0;
    var Month = 0;
    var Day = 0;
    var DayOfWeek = 0;
    var Hour = 0;
    var Minute = 0;
    var Second = 0;
    var MilliSecond = 0;
    function StoreFormat(FormatStr, Nesting, TimeFlag) {
      var Token = "";
      var lastformattoken = "";
      var prevlasttoken = "";
      var Count = 0;
      var Clock12 = false;
      var tmp = 0;
      var isInterval = false;
      var P = 0;
      var FormatCurrent = 0;
      var FormatEnd = 0;
      if (Nesting > 1) return;
      FormatCurrent = 1;
      FormatEnd = FormatStr.length;
      Clock12 = false;
      isInterval = false;
      P = 1;
      while (P <= FormatEnd) {
        Token = FormatStr.charAt(P - 1);
        var $tmp = Token;
        if (($tmp === "'") || ($tmp === '"')) {
          P += 1;
          while ((P < FormatEnd) && (FormatStr.charAt(P - 1) !== Token)) P += 1;
        } else if (($tmp === "A") || ($tmp === "a")) {
          if (($mod.CompareText(pas.System.Copy(FormatStr,P,3),"A\/P") === 0) || ($mod.CompareText(pas.System.Copy(FormatStr,P,4),"AMPM") === 0) || ($mod.CompareText(pas.System.Copy(FormatStr,P,5),"AM\/PM") === 0)) {
            Clock12 = true;
            break;
          };
        };
        P += 1;
      };
      Token = "ÿ";
      lastformattoken = " ";
      prevlasttoken = "H";
      while (FormatCurrent <= FormatEnd) {
        Token = $mod.UpperCase(FormatStr.charAt(FormatCurrent - 1)).charAt(0);
        Count = 1;
        P = FormatCurrent + 1;
        var $tmp1 = Token;
        if (($tmp1 === "'") || ($tmp1 === '"')) {
          while ((P < FormatEnd) && (FormatStr.charAt(P - 1) !== Token)) P += 1;
          P += 1;
          Count = P - FormatCurrent;
          StoreStr(FormatCurrent + 1,Count - 2);
        } else if ($tmp1 === "A") {
          if ($mod.CompareText(pas.System.Copy(FormatStr,FormatCurrent,4),"AMPM") === 0) {
            Count = 4;
            if (Hour < 12) {
              StoreString(aSettings.TimeAMString)}
             else StoreString(aSettings.TimePMString);
          } else if ($mod.CompareText(pas.System.Copy(FormatStr,FormatCurrent,5),"AM\/PM") === 0) {
            Count = 5;
            if (Hour < 12) {
              StoreStr(FormatCurrent,2)}
             else StoreStr(FormatCurrent + 3,2);
          } else if ($mod.CompareText(pas.System.Copy(FormatStr,FormatCurrent,3),"A\/P") === 0) {
            Count = 3;
            if (Hour < 12) {
              StoreStr(FormatCurrent,1)}
             else StoreStr(FormatCurrent + 2,1);
          } else throw $mod.EConvertError.$create("Create$1",["Illegal character in format string"]);
        } else if ($tmp1 === "\/") {
          StoreString(aSettings.DateSeparator);
        } else if ($tmp1 === ":") {
          StoreString(aSettings.TimeSeparator)}
         else if (($tmp1 === " ") || ($tmp1 === "C") || ($tmp1 === "D") || ($tmp1 === "H") || ($tmp1 === "M") || ($tmp1 === "N") || ($tmp1 === "S") || ($tmp1 === "T") || ($tmp1 === "Y") || ($tmp1 === "Z") || ($tmp1 === "F")) {
          while ((P <= FormatEnd) && ($mod.UpperCase(FormatStr.charAt(P - 1)) === Token)) P += 1;
          Count = P - FormatCurrent;
          var $tmp2 = Token;
          if ($tmp2 === " ") {
            StoreStr(FormatCurrent,Count)}
           else if ($tmp2 === "Y") {
            if (Count > 2) {
              StoreInt(Year,4)}
             else StoreInt(Year % 100,2);
          } else if ($tmp2 === "M") {
            if (isInterval && ((prevlasttoken === "H") || TimeFlag)) {
              StoreInt(Minute + ((Hour + (pas.System.Trunc(Math.abs(DateTime)) * 24)) * 60),0)}
             else if ((lastformattoken === "H") || TimeFlag) {
              if (Count === 1) {
                StoreInt(Minute,0)}
               else StoreInt(Minute,2);
            } else {
              var $tmp3 = Count;
              if ($tmp3 === 1) {
                StoreInt(Month,0)}
               else if ($tmp3 === 2) {
                StoreInt(Month,2)}
               else if ($tmp3 === 3) {
                StoreString(aSettings.ShortMonthNames[Month - 1])}
               else {
                StoreString(aSettings.LongMonthNames[Month - 1]);
              };
            };
          } else if ($tmp2 === "D") {
            var $tmp4 = Count;
            if ($tmp4 === 1) {
              StoreInt(Day,0)}
             else if ($tmp4 === 2) {
              StoreInt(Day,2)}
             else if ($tmp4 === 3) {
              StoreString(aSettings.ShortDayNames[DayOfWeek - 1])}
             else if ($tmp4 === 4) {
              StoreString(aSettings.LongDayNames[DayOfWeek - 1])}
             else if ($tmp4 === 5) {
              StoreFormat(aSettings.ShortDateFormat,Nesting + 1,false)}
             else {
              StoreFormat(aSettings.LongDateFormat,Nesting + 1,false);
            };
          } else if ($tmp2 === "H") {
            if (isInterval) {
              StoreInt(Hour + (pas.System.Trunc(Math.abs(DateTime)) * 24),0)}
             else if (Clock12) {
              tmp = Hour % 12;
              if (tmp === 0) tmp = 12;
              if (Count === 1) {
                StoreInt(tmp,0)}
               else StoreInt(tmp,2);
            } else {
              if (Count === 1) {
                StoreInt(Hour,0)}
               else StoreInt(Hour,2);
            }}
           else if ($tmp2 === "N") {
            if (isInterval) {
              StoreInt(Minute + ((Hour + (pas.System.Trunc(Math.abs(DateTime)) * 24)) * 60),0)}
             else if (Count === 1) {
              StoreInt(Minute,0)}
             else StoreInt(Minute,2)}
           else if ($tmp2 === "S") {
            if (isInterval) {
              StoreInt(Second + ((Minute + ((Hour + (pas.System.Trunc(Math.abs(DateTime)) * 24)) * 60)) * 60),0)}
             else if (Count === 1) {
              StoreInt(Second,0)}
             else StoreInt(Second,2)}
           else if ($tmp2 === "Z") {
            if (Count === 1) {
              StoreInt(MilliSecond,0)}
             else StoreInt(MilliSecond,3)}
           else if ($tmp2 === "T") {
            if (Count === 1) {
              StoreFormat(aSettings.ShortTimeFormat,Nesting + 1,true)}
             else StoreFormat(aSettings.LongTimeFormat,Nesting + 1,true)}
           else if ($tmp2 === "C") {
            StoreFormat(aSettings.ShortDateFormat,Nesting + 1,false);
            if ((Hour !== 0) || (Minute !== 0) || (Second !== 0)) {
              StoreString(" ");
              StoreFormat(aSettings.LongTimeFormat,Nesting + 1,true);
            };
          } else if ($tmp2 === "F") {
            StoreFormat(aSettings.ShortDateFormat,Nesting + 1,false);
            StoreString(" ");
            StoreFormat(aSettings.LongTimeFormat,Nesting + 1,true);
          };
          prevlasttoken = lastformattoken;
          lastformattoken = Token;
        } else {
          StoreString(Token);
        };
        FormatCurrent += Count;
      };
    };
    $mod.DecodeDateFully(DateTime,{get: function () {
        return Year;
      }, set: function (v) {
        Year = v;
      }},{get: function () {
        return Month;
      }, set: function (v) {
        Month = v;
      }},{get: function () {
        return Day;
      }, set: function (v) {
        Day = v;
      }},{get: function () {
        return DayOfWeek;
      }, set: function (v) {
        DayOfWeek = v;
      }});
    $mod.DecodeTime(DateTime,{get: function () {
        return Hour;
      }, set: function (v) {
        Hour = v;
      }},{get: function () {
        return Minute;
      }, set: function (v) {
        Minute = v;
      }},{get: function () {
        return Second;
      }, set: function (v) {
        Second = v;
      }},{get: function () {
        return MilliSecond;
      }, set: function (v) {
        MilliSecond = v;
      }});
    if (FormatStr !== "") {
      StoreFormat(FormatStr,0,false)}
     else StoreFormat("C",0,false);
    return Result;
  };
  this.CurrencyFormat = 0;
  this.NegCurrFormat = 0;
  this.CurrencyDecimals = 0;
  this.CurrencyString = "";
  $mod.$implcode = function () {
    $impl.DefaultShortMonthNames = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"];
    $impl.DefaultLongMonthNames = ["January","February","March","April","May","June","July","August","September","October","November","December"];
    $impl.DefaultShortDayNames = ["Sun","Mon","Tue","Wed","Thu","Fri","Sat"];
    $impl.DefaultLongDayNames = ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"];
    $impl.DoShowException = function (S) {
      if ($mod.OnShowException != null) {
        $mod.OnShowException(S)}
       else {
        window.alert(S);
      };
    };
    $impl.OnPascalException = null;
    $impl.OnJSException = null;
    $impl.RTLExceptionHook = function (aError) {
      var S = "";
      if (pas.JS.isClassInstance(aError)) {
        if ($impl.OnPascalException != null) {
          $impl.OnPascalException(rtl.getObject(aError))}
         else $mod.ShowException(rtl.getObject(aError),null);
      } else if (rtl.isObject(aError)) {
        if ($impl.OnJSException != null) {
          $impl.OnJSException(aError)}
         else {
          if (aError.hasOwnProperty("message")) {
            S = rtl.getResStr($mod,"SErrUnknownExceptionType") + ("" + aError["message"])}
           else S = rtl.getResStr($mod,"SErrUnknownExceptionType") + aError.toString();
          $impl.DoShowException(S);
        };
      } else {
        S = rtl.getResStr($mod,"SErrUnknownExceptionType") + ("" + aError);
        $impl.DoShowException(S);
      };
    };
    $impl.feInvalidFormat = 1;
    $impl.feMissingArgument = 2;
    $impl.feInvalidArgIndex = 3;
    $impl.DoFormatError = function (ErrCode, fmt) {
      var $tmp = ErrCode;
      if ($tmp === 1) {
        throw $mod.EConvertError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SInvalidFormat"),pas.System.VarRecs(18,fmt)])}
       else if ($tmp === 2) {
        throw $mod.EConvertError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SArgumentMissing"),pas.System.VarRecs(18,fmt)])}
       else if ($tmp === 3) throw $mod.EConvertError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SInvalidArgIndex"),pas.System.VarRecs(18,fmt)]);
    };
    $impl.maxdigits = 15;
    $impl.ReplaceDecimalSep = function (S, DS) {
      var Result = "";
      var P = 0;
      P = pas.System.Pos(".",S);
      if (P > 0) {
        Result = pas.System.Copy(S,1,P - 1) + DS + pas.System.Copy(S,P + 1,S.length - P)}
       else Result = S;
      return Result;
    };
    $impl.FormatGeneralFloat = function (Value, Precision, DS) {
      var Result = "";
      var P = 0;
      var PE = 0;
      var Q = 0;
      var Exponent = 0;
      if ((Precision === -1) || (Precision > 15)) Precision = 15;
      Result = rtl.floatToStr(Value,Precision + 7);
      Result = $mod.TrimLeft(Result);
      P = pas.System.Pos(".",Result);
      if (P === 0) return Result;
      PE = pas.System.Pos("E",Result);
      if (PE === 0) {
        Result = $impl.ReplaceDecimalSep(Result,DS);
        return Result;
      };
      Q = PE + 2;
      Exponent = 0;
      while (Q <= Result.length) {
        Exponent = ((Exponent * 10) + Result.charCodeAt(Q - 1)) - 48;
        Q += 1;
      };
      if (Result.charAt((PE + 1) - 1) === "-") Exponent = -Exponent;
      if (((P + Exponent) < PE) && (Exponent > -6)) {
        Result = rtl.strSetLength(Result,PE - 1);
        if (Exponent >= 0) {
          for (var $l = 0, $end = Exponent - 1; $l <= $end; $l++) {
            Q = $l;
            Result = rtl.setCharAt(Result,P - 1,Result.charAt((P + 1) - 1));
            P += 1;
          };
          Result = rtl.setCharAt(Result,P - 1,".");
          P = 1;
          if (Result.charAt(P - 1) === "-") P += 1;
          while ((Result.charAt(P - 1) === "0") && (P < Result.length) && (pas.System.Copy(Result,P + 1,DS.length) !== DS)) pas.System.Delete({get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},P,1);
        } else {
          pas.System.Insert(pas.System.Copy("00000",1,-Exponent),{get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},P - 1);
          Result = rtl.setCharAt(Result,P - Exponent - 1,Result.charAt(P - Exponent - 1 - 1));
          Result = rtl.setCharAt(Result,P - 1,".");
          if (Exponent !== -1) Result = rtl.setCharAt(Result,P - Exponent - 1 - 1,"0");
        };
        Q = Result.length;
        while ((Q > 0) && (Result.charAt(Q - 1) === "0")) Q -= 1;
        if (Result.charAt(Q - 1) === ".") Q -= 1;
        if ((Q === 0) || ((Q === 1) && (Result.charAt(0) === "-"))) {
          Result = "0"}
         else Result = rtl.strSetLength(Result,Q);
      } else {
        while (Result.charAt(PE - 1 - 1) === "0") {
          pas.System.Delete({get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},PE - 1,1);
          PE -= 1;
        };
        if (Result.charAt(PE - 1 - 1) === DS) {
          pas.System.Delete({get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},PE - 1,1);
          PE -= 1;
        };
        if (Result.charAt((PE + 1) - 1) === "+") {
          pas.System.Delete({get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},PE + 1,1)}
         else PE += 1;
        while (Result.charAt((PE + 1) - 1) === "0") pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},PE + 1,1);
      };
      Result = $impl.ReplaceDecimalSep(Result,DS);
      return Result;
    };
    $impl.FormatExponentFloat = function (Value, Precision, Digits, DS) {
      var Result = "";
      var P = 0;
      DS = $mod.FormatSettings.DecimalSeparator;
      if ((Precision === -1) || (Precision > 15)) Precision = 15;
      Result = rtl.floatToStr(Value,Precision + 7);
      while (Result.charAt(0) === " ") pas.System.Delete({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},1,1);
      P = pas.System.Pos("E",Result);
      if (P === 0) {
        Result = $impl.ReplaceDecimalSep(Result,DS);
        return Result;
      };
      P += 2;
      if (Digits > 4) Digits = 4;
      Digits = (Result.length - P - Digits) + 1;
      if (Digits < 0) {
        pas.System.Insert(pas.System.Copy("0000",1,-Digits),{get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},P)}
       else while ((Digits > 0) && (Result.charAt(P - 1) === "0")) {
        pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},P,1);
        if (P > Result.length) {
          pas.System.Delete({get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},P - 2,2);
          break;
        };
        Digits -= 1;
      };
      Result = $impl.ReplaceDecimalSep(Result,DS);
      return Result;
    };
    $impl.FormatFixedFloat = function (Value, Digits, DS) {
      var Result = "";
      if (Digits === -1) {
        Digits = 2}
       else if (Digits > 18) Digits = 18;
      Result = rtl.floatToStr(Value,0,Digits);
      if ((Result !== "") && (Result.charAt(0) === " ")) pas.System.Delete({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},1,1);
      Result = $impl.ReplaceDecimalSep(Result,DS);
      return Result;
    };
    $impl.FormatNumberFloat = function (Value, Digits, DS, TS) {
      var Result = "";
      var P = 0;
      if (Digits === -1) {
        Digits = 2}
       else if (Digits > 15) Digits = 15;
      Result = rtl.floatToStr(Value,0,Digits);
      if ((Result !== "") && (Result.charAt(0) === " ")) pas.System.Delete({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},1,1);
      P = pas.System.Pos(".",Result);
      if (P <= 0) P = Result.length + 1;
      Result = $impl.ReplaceDecimalSep(Result,DS);
      P -= 3;
      if ((TS !== "") && (TS !== "\x00")) while (P > 1) {
        if (Result.charAt(P - 1 - 1) !== "-") pas.System.Insert(TS,{get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},P);
        P -= 3;
      };
      return Result;
    };
    $impl.RemoveLeadingNegativeSign = function (AValue, DS, aThousandSeparator) {
      var Result = false;
      var i = 0;
      var TS = "";
      var StartPos = 0;
      Result = false;
      StartPos = 2;
      TS = aThousandSeparator;
      for (var $l = StartPos, $end = AValue.get().length; $l <= $end; $l++) {
        i = $l;
        Result = (AValue.get().charCodeAt(i - 1) in rtl.createSet(48,DS.charCodeAt(),69,43)) || (AValue.get().charAt(i - 1) === TS);
        if (!Result) break;
      };
      if (Result && (AValue.get().charAt(0) === "-")) pas.System.Delete(AValue,1,1);
      return Result;
    };
    $impl.FormatNumberCurrency = function (Value, Digits, aSettings) {
      var Result = "";
      var Negative = false;
      var P = 0;
      var CS = "";
      var DS = "";
      var TS = "";
      DS = aSettings.DecimalSeparator;
      TS = aSettings.ThousandSeparator;
      CS = aSettings.CurrencyString;
      if (Digits === -1) {
        Digits = aSettings.CurrencyDecimals}
       else if (Digits > 18) Digits = 18;
      Result = rtl.floatToStr(Value / 10000,0,Digits);
      Negative = Result.charAt(0) === "-";
      if (Negative) pas.System.Delete({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},1,1);
      P = pas.System.Pos(".",Result);
      if (TS !== "") {
        if (P !== 0) {
          Result = $impl.ReplaceDecimalSep(Result,DS)}
         else P = Result.length + 1;
        P -= 3;
        while (P > 1) {
          pas.System.Insert(TS,{get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},P);
          P -= 3;
        };
      };
      if (Negative) $impl.RemoveLeadingNegativeSign({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},DS,TS);
      if (!Negative) {
        var $tmp = aSettings.CurrencyFormat;
        if ($tmp === 0) {
          Result = CS + Result}
         else if ($tmp === 1) {
          Result = Result + CS}
         else if ($tmp === 2) {
          Result = CS + " " + Result}
         else if ($tmp === 3) Result = Result + " " + CS;
      } else {
        var $tmp1 = aSettings.NegCurrFormat;
        if ($tmp1 === 0) {
          Result = "(" + CS + Result + ")"}
         else if ($tmp1 === 1) {
          Result = "-" + CS + Result}
         else if ($tmp1 === 2) {
          Result = CS + "-" + Result}
         else if ($tmp1 === 3) {
          Result = CS + Result + "-"}
         else if ($tmp1 === 4) {
          Result = "(" + Result + CS + ")"}
         else if ($tmp1 === 5) {
          Result = "-" + Result + CS}
         else if ($tmp1 === 6) {
          Result = Result + "-" + CS}
         else if ($tmp1 === 7) {
          Result = Result + CS + "-"}
         else if ($tmp1 === 8) {
          Result = "-" + Result + " " + CS}
         else if ($tmp1 === 9) {
          Result = "-" + CS + " " + Result}
         else if ($tmp1 === 10) {
          Result = Result + " " + CS + "-"}
         else if ($tmp1 === 11) {
          Result = CS + " " + Result + "-"}
         else if ($tmp1 === 12) {
          Result = CS + " " + "-" + Result}
         else if ($tmp1 === 13) {
          Result = Result + "-" + " " + CS}
         else if ($tmp1 === 14) {
          Result = "(" + CS + " " + Result + ")"}
         else if ($tmp1 === 15) Result = "(" + Result + " " + CS + ")";
      };
      return Result;
    };
    $impl.RESpecials = "([\\$\\+\\[\\]\\(\\)\\\\\\.\\*\\^\\?\\|])";
    $impl.DateTimeToStrFormat = ["c","f"];
    var WhiteSpace = " \b\t\n\f\r";
    var Digits = "0123456789";
    $impl.IntStrToDate = function (ErrorMsg, S, useformat, separator) {
      var Result = 0.0;
      function FixErrorMsg(errmarg) {
        ErrorMsg.set($mod.Format(rtl.getResStr(pas.RTLConsts,"SInvalidDateFormat"),pas.System.VarRecs(18,errmarg)));
      };
      var df = "";
      var d = 0;
      var m = 0;
      var y = 0;
      var ly = 0;
      var ld = 0;
      var lm = 0;
      var n = 0;
      var i = 0;
      var len = 0;
      var c = 0;
      var dp = 0;
      var mp = 0;
      var yp = 0;
      var which = 0;
      var s1 = "";
      var values = [];
      var YearMoreThenTwoDigits = false;
      values = rtl.arraySetLength(values,0,4);
      Result = 0;
      len = S.length;
      ErrorMsg.set("");
      while ((len > 0) && (pas.System.Pos(S.charAt(len - 1),WhiteSpace) > 0)) len -= 1;
      if (len === 0) {
        FixErrorMsg(S);
        return Result;
      };
      YearMoreThenTwoDigits = false;
      if (separator === "\x00") if ($mod.FormatSettings.DateSeparator !== "\x00") {
        separator = $mod.FormatSettings.DateSeparator}
       else separator = "-";
      df = $mod.UpperCase(useformat);
      yp = 0;
      mp = 0;
      dp = 0;
      which = 0;
      i = 0;
      while ((i < df.length) && (which < 3)) {
        i += 1;
        var $tmp = df.charAt(i - 1);
        if ($tmp === "Y") {
          if (yp === 0) {
            which += 1;
            yp = which;
          }}
         else if ($tmp === "M") {
          if (mp === 0) {
            which += 1;
            mp = which;
          }}
         else if ($tmp === "D") if (dp === 0) {
          which += 1;
          dp = which;
        };
      };
      for (i = 1; i <= 3; i++) values[i] = 0;
      s1 = "";
      n = 0;
      for (var $l = 1, $end = len; $l <= $end; $l++) {
        i = $l;
        if (pas.System.Pos(S.charAt(i - 1),Digits) > 0) s1 = s1 + S.charAt(i - 1);
        if ((separator !== " ") && (S.charAt(i - 1) === " ")) continue;
        if ((S.charAt(i - 1) === separator) || ((i === len) && (pas.System.Pos(S.charAt(i - 1),Digits) > 0))) {
          n += 1;
          if (n > 3) {
            FixErrorMsg(S);
            return Result;
          };
          if ((n === yp) && (s1.length > 2)) YearMoreThenTwoDigits = true;
          pas.System.val$6(s1,{a: n, p: values, get: function () {
              return this.p[this.a];
            }, set: function (v) {
              this.p[this.a] = v;
            }},{get: function () {
              return c;
            }, set: function (v) {
              c = v;
            }});
          if (c !== 0) {
            FixErrorMsg(S);
            return Result;
          };
          s1 = "";
        } else if (pas.System.Pos(S.charAt(i - 1),Digits) === 0) {
          FixErrorMsg(S);
          return Result;
        };
      };
      if ((which < 3) && (n > which)) {
        FixErrorMsg(S);
        return Result;
      };
      $mod.DecodeDate($mod.Date(),{get: function () {
          return ly;
        }, set: function (v) {
          ly = v;
        }},{get: function () {
          return lm;
        }, set: function (v) {
          lm = v;
        }},{get: function () {
          return ld;
        }, set: function (v) {
          ld = v;
        }});
      if (n === 3) {
        y = values[yp];
        m = values[mp];
        d = values[dp];
      } else {
        y = ly;
        if (n < 2) {
          d = values[1];
          m = lm;
        } else if (dp < mp) {
          d = values[1];
          m = values[2];
        } else {
          d = values[2];
          m = values[1];
        };
      };
      if ((y >= 0) && (y < 100) && !YearMoreThenTwoDigits) {
        ly = ly - $mod.TwoDigitYearCenturyWindow;
        y += rtl.trunc(ly / 100) * 100;
        if (($mod.TwoDigitYearCenturyWindow > 0) && (y < ly)) y += 100;
      };
      if (!$mod.TryEncodeDate(y,m,d,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }})) ErrorMsg.set(rtl.getResStr(pas.RTLConsts,"SErrInvalidDate"));
      return Result;
    };
    var AMPM_None = 0;
    var AMPM_AM = 1;
    var AMPM_PM = 2;
    var tiHour = 0;
    var tiMin = 1;
    var tiSec = 2;
    var tiMSec = 3;
    var Digits$1 = "0123456789";
    $impl.IntStrToTime = function (ErrorMsg, S, Len, aSettings) {
      var Result = 0.0;
      var AmPm = 0;
      var TimeValues = [];
      function SplitElements(TimeValues, AmPm) {
        var Result = false;
        var Cur = 0;
        var Offset = 0;
        var ElemLen = 0;
        var Err = 0;
        var TimeIndex = 0;
        var FirstSignificantDigit = 0;
        var Value = 0;
        var DigitPending = false;
        var MSecPending = false;
        var AmPmStr = "";
        var CurChar = "";
        var I = 0;
        var allowedchars = "";
        Result = false;
        AmPm.set(0);
        MSecPending = false;
        TimeIndex = 0;
        for (I = 0; I <= 3; I++) TimeValues.get()[I] = 0;
        Cur = 1;
        while ((Cur < Len) && (S.charAt(Cur - 1) === " ")) Cur += 1;
        Offset = Cur;
        if ((Cur > (Len - 1)) || (S.charAt(Cur - 1) === aSettings.TimeSeparator) || (S.charAt(Cur - 1) === aSettings.DecimalSeparator)) {
          return Result;
        };
        DigitPending = pas.System.Pos(S.charAt(Cur - 1),Digits$1) > 0;
        while (Cur <= Len) {
          CurChar = S.charAt(Cur - 1);
          if (pas.System.Pos(CurChar,Digits$1) > 0) {
            if (!DigitPending || (TimeIndex > 3)) {
              return Result;
            };
            Offset = Cur;
            if (CurChar !== "0") {
              FirstSignificantDigit = Offset}
             else FirstSignificantDigit = -1;
            while ((Cur < Len) && (pas.System.Pos(S.charAt((Cur + 1) - 1),Digits$1) > 0)) {
              if ((FirstSignificantDigit === -1) && (S.charAt(Cur - 1) !== "0")) FirstSignificantDigit = Cur;
              Cur += 1;
            };
            if (FirstSignificantDigit === -1) FirstSignificantDigit = Cur;
            ElemLen = (1 + Cur) - FirstSignificantDigit;
            if ((ElemLen <= 2) || ((ElemLen <= 3) && (TimeIndex === 3))) {
              pas.System.val$6(pas.System.Copy(S,FirstSignificantDigit,ElemLen),{get: function () {
                  return Value;
                }, set: function (v) {
                  Value = v;
                }},{get: function () {
                  return Err;
                }, set: function (v) {
                  Err = v;
                }});
              TimeValues.get()[TimeIndex] = Value;
              TimeIndex += 1;
              DigitPending = false;
            } else {
              return Result;
            };
          } else if (CurChar === " ") {}
          else if (CurChar === aSettings.TimeSeparator) {
            if (DigitPending || (TimeIndex > 2)) {
              return Result;
            };
            DigitPending = true;
            MSecPending = false;
          } else if (CurChar === aSettings.DecimalSeparator) {
            if (DigitPending || MSecPending || (TimeIndex !== 3)) {
              return Result;
            };
            DigitPending = true;
            MSecPending = true;
          } else {
            if ((AmPm.get() !== 0) || DigitPending) {
              return Result;
            };
            Offset = Cur;
            allowedchars = aSettings.DecimalSeparator + " ";
            if (aSettings.TimeSeparator !== "\x00") allowedchars = allowedchars + aSettings.TimeSeparator;
            while ((Cur < Len) && (pas.System.Pos(S.charAt((Cur + 1) - 1),allowedchars) === 0) && (pas.System.Pos(S.charAt((Cur + 1) - 1),Digits$1) === 0)) Cur += 1;
            ElemLen = (1 + Cur) - Offset;
            AmPmStr = pas.System.Copy(S,Offset,ElemLen);
            if ($mod.CompareText(AmPmStr,aSettings.TimeAMString) === 0) {
              AmPm.set(1)}
             else if ($mod.CompareText(AmPmStr,aSettings.TimePMString) === 0) {
              AmPm.set(2)}
             else if ($mod.CompareText(AmPmStr,"AM") === 0) {
              AmPm.set(1)}
             else if ($mod.CompareText(AmPmStr,"PM") === 0) {
              AmPm.set(2)}
             else {
              return Result;
            };
            if (TimeIndex === 0) {
              DigitPending = true;
            } else {
              TimeIndex = 3 + 1;
              DigitPending = false;
            };
          };
          Cur += 1;
        };
        if ((TimeIndex === 0) || ((AmPm.get() !== 0) && ((TimeValues.get()[0] > 12) || (TimeValues.get()[0] === 0))) || DigitPending) return Result;
        Result = true;
        return Result;
      };
      TimeValues = rtl.arraySetLength(TimeValues,0,4);
      AmPm = 0;
      if (!SplitElements({get: function () {
          return TimeValues;
        }, set: function (v) {
          TimeValues = v;
        }},{get: function () {
          return AmPm;
        }, set: function (v) {
          AmPm = v;
        }})) {
        ErrorMsg.set($mod.Format(rtl.getResStr(pas.RTLConsts,"SErrInvalidTimeFormat"),pas.System.VarRecs(18,S)));
        return Result;
      };
      if ((AmPm === 2) && (TimeValues[0] !== 12)) {
        TimeValues[0] += 12}
       else if ((AmPm === 1) && (TimeValues[0] === 12)) TimeValues[0] = 0;
      if (!$mod.TryEncodeTime(TimeValues[0],TimeValues[1],TimeValues[2],TimeValues[3],{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }})) ErrorMsg.set($mod.Format(rtl.getResStr(pas.RTLConsts,"SErrInvalidTimeFormat"),pas.System.VarRecs(18,S)));
      return Result;
    };
    $impl.IntTryStrToInt = function (S, res, aSep) {
      var Result = false;
      var Radix = 10;
      var N = "";
      var J = undefined;
      N = S;
      if ((pas.System.Pos(aSep,N) !== 0) || (pas.System.Pos(".",N) !== 0)) return false;
      var $tmp = pas.System.Copy(N,1,1);
      if ($tmp === "$") {
        Radix = 16}
       else if ($tmp === "&") {
        Radix = 8}
       else if ($tmp === "%") Radix = 2;
      if ((Radix !== 16) && (pas.System.Pos("e",$mod.LowerCase(N)) !== 0)) return false;
      if (Radix !== 10) pas.System.Delete({get: function () {
          return N;
        }, set: function (v) {
          N = v;
        }},1,1);
      J = parseInt(N,Radix);
      Result = !isNaN(J);
      if (Result) res.set(rtl.trunc(J));
      return Result;
    };
    $mod.$resourcestrings = {SApplicationException: {org: "Application raised an exception: "}, SErrUnknownExceptionType: {org: "Caught unknown exception type : "}};
  };
  $mod.$init = function () {
    $mod.ShortMonthNames = $impl.DefaultShortMonthNames.slice(0);
    $mod.LongMonthNames = $impl.DefaultLongMonthNames.slice(0);
    $mod.ShortDayNames = $impl.DefaultShortDayNames.slice(0);
    $mod.LongDayNames = $impl.DefaultLongDayNames.slice(0);
    $mod.FormatSettings.$assign($mod.TFormatSettings.Create());
    $mod.TimeSeparator = $mod.FormatSettings.TimeSeparator;
    $mod.DateSeparator = $mod.FormatSettings.DateSeparator;
    $mod.ShortDateFormat = $mod.FormatSettings.ShortDateFormat;
    $mod.LongDateFormat = $mod.FormatSettings.LongDateFormat;
    $mod.ShortTimeFormat = $mod.FormatSettings.ShortTimeFormat;
    $mod.LongTimeFormat = $mod.FormatSettings.LongTimeFormat;
    $mod.DecimalSeparator = $mod.FormatSettings.DecimalSeparator;
    $mod.ThousandSeparator = $mod.FormatSettings.ThousandSeparator;
    $mod.TimeAMString = $mod.FormatSettings.TimeAMString;
    $mod.TimePMString = $mod.FormatSettings.TimePMString;
    $mod.CurrencyFormat = $mod.FormatSettings.CurrencyFormat;
    $mod.NegCurrFormat = $mod.FormatSettings.NegCurrFormat;
    $mod.CurrencyDecimals = $mod.FormatSettings.CurrencyDecimals;
    $mod.CurrencyString = $mod.FormatSettings.CurrencyString;
  };
},[]);
rtl.module("TypInfo",["System","SysUtils","Types","RTLConsts","JS"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.pfGetFunction = 1;
  this.pfSetProcedure = 2;
  this.pfHasIndex = 16;
  rtl.createClass(this,"EPropertyError",pas.SysUtils.Exception,function () {
  });
  this.GetPropInfo = function (TI, PropName) {
    var Result = null;
    var m = null;
    var i = 0;
    var C = null;
    C = TI;
    while (C !== null) {
      m = C.members[PropName];
      if (rtl.isExt(m,rtl.tTypeMemberProperty)) return m;
      if (!rtl.isExt(C,rtl.tTypeInfoClass)) break;
      C = C.ancestor;
    };
    Result = null;
    do {
      for (var $l = 0, $end = TI.properties.length - 1; $l <= $end; $l++) {
        i = $l;
        if (pas.SysUtils.CompareText(PropName,TI.properties[i]) === 0) {
          m = TI.members[TI.properties[i]];
          if (rtl.isExt(m,rtl.tTypeMemberProperty)) Result = m;
          return Result;
        };
      };
      if (!rtl.isExt(TI,rtl.tTypeInfoClass)) break;
      TI = TI.ancestor;
    } while (!(TI === null));
    return Result;
  };
  this.GetPropInfo$1 = function (TI, PropName, Kinds) {
    var Result = null;
    Result = $mod.GetPropInfo(TI,PropName);
    if (rtl.neSet(Kinds,{}) && (Result !== null) && !(Result.typeinfo.kind in Kinds)) Result = null;
    return Result;
  };
  this.GetPropInfo$4 = function (aClass, PropName) {
    var Result = null;
    Result = $mod.GetPropInfo$1(aClass.$rtti,PropName,{});
    return Result;
  };
  this.GetJSValueProp$1 = function (Instance, PropInfo) {
    var Result = undefined;
    var gk = 0;
    gk = $impl.GetPropGetterKind(PropInfo);
    var $tmp = gk;
    if ($tmp === 0) {
      throw $mod.EPropertyError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SCantReadPropertyS"),pas.System.VarRecs(18,PropInfo.name)])}
     else if ($tmp === 1) {
      Result = Instance[PropInfo.getter]}
     else if ($tmp === 2) {
      if ((16 & PropInfo.flags) > 0) {
        Result = Instance[PropInfo.getter](PropInfo.index)}
       else Result = Instance[PropInfo.getter]()}
     else if ($tmp === 3) throw $mod.EPropertyError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SIndexedPropertyNeedsParams"),pas.System.VarRecs(18,PropInfo.name)]);
    return Result;
  };
  this.GetJSValueProp$3 = function (Instance, PropInfo) {
    var Result = undefined;
    Result = $mod.GetJSValueProp$1(Instance,PropInfo);
    return Result;
  };
  this.SetJSValueProp$1 = function (Instance, PropInfo, Value) {
    var sk = 0;
    sk = $impl.GetPropSetterKind(PropInfo);
    var $tmp = sk;
    if ($tmp === 0) {
      throw $mod.EPropertyError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SCantWritePropertyS"),pas.System.VarRecs(18,PropInfo.name)])}
     else if ($tmp === 1) {
      Instance[PropInfo.setter] = Value}
     else if ($tmp === 2) {
      if ((16 & PropInfo.flags) > 0) {
        Instance[PropInfo.setter](PropInfo.index,Value)}
       else Instance[PropInfo.setter](Value)}
     else if ($tmp === 3) throw $mod.EPropertyError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SIndexedPropertyNeedsParams"),pas.System.VarRecs(18,PropInfo.name)]);
  };
  this.SetJSValueProp$3 = function (Instance, PropInfo, Value) {
    $mod.SetJSValueProp$1(Instance,PropInfo,Value);
  };
  this.SetOrdProp$1 = function (Instance, PropInfo, Value) {
    var o = null;
    var i = 0;
    if (PropInfo.typeinfo.kind === 5) {
      o = new Object();
      for (i = 0; i <= 31; i++) if (((1 << i) & Value) > 0) o["" + i] = true;
      $mod.SetJSValueProp$3(Instance,PropInfo,o);
    } else if (PropInfo.typeinfo.kind === 2) {
      $mod.SetJSValueProp$3(Instance,PropInfo,String.fromCharCode(Value))}
     else $mod.SetJSValueProp$3(Instance,PropInfo,Value);
  };
  this.GetEnumValue = function (TypeInfo, Name) {
    var Result = 0;
    Result = TypeInfo.enumtype[Name];
    return Result;
  };
  this.SetStrProp$1 = function (Instance, PropInfo, Value) {
    $mod.SetJSValueProp$3(Instance,PropInfo,Value);
  };
  this.SetFloatProp$1 = function (Instance, PropInfo, Value) {
    $mod.SetJSValueProp$3(Instance,PropInfo,Value);
  };
  this.GetObjectProp$2 = function (Instance, PropInfo) {
    var Result = null;
    Result = $mod.GetObjectProp$3(Instance,PropInfo,null);
    return Result;
  };
  this.GetObjectProp$3 = function (Instance, PropInfo, MinClass) {
    var Result = null;
    var O = null;
    O = rtl.getObject($mod.GetJSValueProp$3(Instance,PropInfo));
    if ((MinClass !== null) && !O.$class.InheritsFrom(MinClass)) {
      Result = null}
     else Result = O;
    return Result;
  };
  this.SetObjectProp$1 = function (Instance, PropInfo, Value) {
    $mod.SetJSValueProp$3(Instance,PropInfo,Value);
  };
  this.SetMethodProp = function (Instance, PropInfo, Value) {
    var cb = null;
    var Code = null;
    Code = Value.Code;
    if (Code === null) {
      cb = null}
     else if (rtl.isFunction(Code)) {
      if ((Code["scope"] === Value.Data) && (rtl.isFunction(Code["fn"]) || rtl.isString(Code["fn"]))) {
        cb = Code;
      } else if (rtl.isString(Code["fn"])) {
        cb = rtl.createCallback(Value.Data,"" + Code["fn"])}
       else cb = rtl.createCallback(Value.Data,Code);
    } else cb = rtl.createCallback(Value.Data,Code);
    $mod.SetJSValueProp$3(Instance,PropInfo,cb);
  };
  this.SetInterfaceProp$1 = function (Instance, PropInfo, Value) {
    var sk = 0;
    var Setter = "";
    if (PropInfo.typeinfo.kind !== 18) throw pas.SysUtils.Exception.$create("Create$1",["Cannot set RAW interface from IInterface interface"]);
    sk = $impl.GetPropSetterKind(PropInfo);
    Setter = PropInfo.setter;
    var $tmp = sk;
    if ($tmp === 0) {
      throw $mod.EPropertyError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SCantWritePropertyS"),pas.System.VarRecs(18,PropInfo.name)])}
     else if ($tmp === 1) {
      rtl.setIntfP(Instance,Setter,Value)}
     else if ($tmp === 2) {
      if ((16 & PropInfo.flags) > 0) {
        Instance[Setter](PropInfo.index,Value)}
       else Instance[Setter](Value)}
     else if ($tmp === 3) throw $mod.EPropertyError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SIndexedPropertyNeedsParams"),pas.System.VarRecs(18,PropInfo.name)]);
  };
  $mod.$implcode = function () {
    $impl.TGetterKind = {"0": "gkNone", gkNone: 0, "1": "gkField", gkField: 1, "2": "gkFunction", gkFunction: 2, "3": "gkFunctionWithParams", gkFunctionWithParams: 3};
    $impl.GetPropGetterKind = function (PropInfo) {
      var Result = 0;
      if (PropInfo.getter === "") {
        Result = 0}
       else if ((1 & PropInfo.flags) > 0) {
        if (rtl.length(PropInfo.params) > 0) {
          Result = 3}
         else Result = 2;
      } else Result = 1;
      return Result;
    };
    $impl.TSetterKind = {"0": "skNone", skNone: 0, "1": "skField", skField: 1, "2": "skProcedure", skProcedure: 2, "3": "skProcedureWithParams", skProcedureWithParams: 3};
    $impl.GetPropSetterKind = function (PropInfo) {
      var Result = 0;
      if (PropInfo.setter === "") {
        Result = 0}
       else if ((2 & PropInfo.flags) > 0) {
        if (rtl.length(PropInfo.params) > 0) {
          Result = 3}
         else Result = 2;
      } else Result = 1;
      return Result;
    };
  };
},[]);
rtl.module("simplelinkedlist",["System"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass(this,"TLinkedListItem",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.Next = null;
    };
    this.$final = function () {
      this.Next = undefined;
      pas.System.TObject.$final.call(this);
    };
  });
  rtl.createClass(this,"TLinkedListVisitor",pas.System.TObject,function () {
  });
  rtl.createClass(this,"TLinkedList",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FItemClass = null;
      this.FRoot = null;
    };
    this.$final = function () {
      this.FItemClass = undefined;
      this.FRoot = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Create$1 = function (AnItemClass) {
      this.FItemClass = AnItemClass;
      return this;
    };
    this.Destroy = function () {
      this.Clear();
      pas.System.TObject.Destroy.call(this);
    };
    this.Clear = function () {
      var I = null;
      I = this.FRoot;
      while (I !== null) {
        this.FRoot = I;
        I = I.Next;
        this.FRoot.Next = null;
        pas.SysUtils.FreeAndNil({p: this, get: function () {
            return this.p.FRoot;
          }, set: function (v) {
            this.p.FRoot = v;
          }});
      };
    };
    this.Add = function () {
      var Result = null;
      Result = this.FItemClass.$create("Create");
      Result.Next = this.FRoot;
      this.FRoot = Result;
      return Result;
    };
    this.ForEach = function (Visitor) {
      var I = null;
      I = this.FRoot;
      while ((I !== null) && Visitor.Visit(I)) I = I.Next;
    };
    this.RemoveItem = function (Item, FreeItem) {
      var I = null;
      if ((Item !== null) && (this.FRoot !== null)) {
        if (Item === this.FRoot) {
          this.FRoot = Item.Next}
         else {
          I = this.FRoot;
          while ((I.Next !== null) && (I.Next !== Item)) I = I.Next;
          if (I.Next === Item) I.Next = Item.Next;
        };
        if (FreeItem) Item = rtl.freeLoc(Item);
      };
    };
  });
},["SysUtils"]);
rtl.module("Classes",["System","RTLConsts","Types","SysUtils","JS","TypInfo"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.$rtti.$MethodVar("TNotifyEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]]]), methodkind: 0});
  rtl.createClass(this,"EStreamError",pas.SysUtils.Exception,function () {
  });
  rtl.createClass(this,"EFilerError",this.EStreamError,function () {
  });
  rtl.createClass(this,"EReadError",this.EFilerError,function () {
  });
  rtl.createClass(this,"EClassNotFound",this.EFilerError,function () {
  });
  rtl.createClass(this,"EResNotFound",pas.SysUtils.Exception,function () {
  });
  rtl.createClass(this,"EListError",pas.SysUtils.Exception,function () {
  });
  rtl.createClass(this,"EStringListError",this.EListError,function () {
  });
  rtl.createClass(this,"EComponentError",pas.SysUtils.Exception,function () {
  });
  rtl.createClass(this,"EParserError",pas.SysUtils.Exception,function () {
  });
  rtl.createClass(this,"EOutOfResources",pas.SysUtils.EOutOfMemory,function () {
  });
  this.TAlignment = {"0": "taLeftJustify", taLeftJustify: 0, "1": "taRightJustify", taRightJustify: 1, "2": "taCenter", taCenter: 2};
  this.$rtti.$Enum("TAlignment",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TAlignment});
  rtl.createClass(this,"TFPList",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FList = [];
      this.FCount = 0;
      this.FCapacity = 0;
    };
    this.$final = function () {
      this.FList = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Get = function (Index) {
      var Result = undefined;
      if ((Index < 0) || (Index >= this.FCount)) this.RaiseIndexError(Index);
      Result = this.FList[Index];
      return Result;
    };
    this.SetCapacity = function (NewCapacity) {
      if (NewCapacity < this.FCount) this.$class.Error(rtl.getResStr(pas.RTLConsts,"SListCapacityError"),"" + NewCapacity);
      if (NewCapacity === this.FCapacity) return;
      this.FList = rtl.arraySetLength(this.FList,undefined,NewCapacity);
      this.FCapacity = NewCapacity;
    };
    this.SetCount = function (NewCount) {
      if (NewCount < 0) this.$class.Error(rtl.getResStr(pas.RTLConsts,"SListCountError"),"" + NewCount);
      if (NewCount > this.FCount) {
        if (NewCount > this.FCapacity) this.SetCapacity(NewCount);
      };
      this.FCount = NewCount;
    };
    this.RaiseIndexError = function (Index) {
      this.$class.Error(rtl.getResStr(pas.RTLConsts,"SListIndexError"),"" + Index);
    };
    this.Destroy = function () {
      this.Clear();
      pas.System.TObject.Destroy.call(this);
    };
    this.Add = function (Item) {
      var Result = 0;
      if (this.FCount === this.FCapacity) this.Expand();
      this.FList[this.FCount] = Item;
      Result = this.FCount;
      this.FCount += 1;
      return Result;
    };
    this.Clear = function () {
      if (rtl.length(this.FList) > 0) {
        this.SetCount(0);
        this.SetCapacity(0);
      };
    };
    this.Delete = function (Index) {
      if ((Index < 0) || (Index >= this.FCount)) this.$class.Error(rtl.getResStr(pas.RTLConsts,"SListIndexError"),"" + Index);
      this.FCount = this.FCount - 1;
      this.FList.splice(Index,1);
      this.FCapacity -= 1;
    };
    this.Error = function (Msg, Data) {
      throw $mod.EListError.$create("CreateFmt",[Msg,pas.System.VarRecs(18,Data)]);
    };
    this.Expand = function () {
      var Result = null;
      var IncSize = 0;
      if (this.FCount < this.FCapacity) return this;
      IncSize = 4;
      if (this.FCapacity > 3) IncSize = IncSize + 4;
      if (this.FCapacity > 8) IncSize = IncSize + 8;
      if (this.FCapacity > 127) IncSize += this.FCapacity >>> 2;
      this.SetCapacity(this.FCapacity + IncSize);
      Result = this;
      return Result;
    };
    this.IndexOf = function (Item) {
      var Result = 0;
      var C = 0;
      Result = 0;
      C = this.FCount;
      while ((Result < C) && (this.FList[Result] != Item)) Result += 1;
      if (Result >= C) Result = -1;
      return Result;
    };
    this.IndexOfItem = function (Item, Direction) {
      var Result = 0;
      if (Direction === 0) {
        Result = this.IndexOf(Item)}
       else {
        Result = this.FCount - 1;
        while ((Result >= 0) && (this.FList[Result] != Item)) Result = Result - 1;
      };
      return Result;
    };
    this.Insert = function (Index, Item) {
      if ((Index < 0) || (Index > this.FCount)) this.$class.Error(rtl.getResStr(pas.RTLConsts,"SListIndexError"),"" + Index);
      this.FList.splice(Index,0,Item);
      this.FCapacity += 1;
      this.FCount += 1;
    };
    this.Last = function () {
      var Result = undefined;
      if (this.FCount === 0) {
        Result = null}
       else Result = this.Get(this.FCount - 1);
      return Result;
    };
    this.Remove = function (Item) {
      var Result = 0;
      Result = this.IndexOf(Item);
      if (Result !== -1) this.Delete(Result);
      return Result;
    };
  });
  this.TListNotification = {"0": "lnAdded", lnAdded: 0, "1": "lnExtracted", lnExtracted: 1, "2": "lnDeleted", lnDeleted: 2};
  rtl.createClass(this,"TList",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FList = null;
    };
    this.$final = function () {
      this.FList = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Get = function (Index) {
      var Result = undefined;
      Result = this.FList.Get(Index);
      return Result;
    };
    this.Notify = function (aValue, Action) {
      if (pas.System.Assigned(aValue)) ;
      if (Action === 1) ;
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FList.FCount;
      return Result;
    };
    this.Create$1 = function () {
      pas.System.TObject.Create.call(this);
      this.FList = $mod.TFPList.$create("Create");
      return this;
    };
    this.Destroy = function () {
      if (this.FList != null) this.Clear();
      pas.SysUtils.FreeAndNil({p: this, get: function () {
          return this.p.FList;
        }, set: function (v) {
          this.p.FList = v;
        }});
    };
    this.Add = function (Item) {
      var Result = 0;
      Result = this.FList.Add(Item);
      if (pas.System.Assigned(Item)) this.Notify(Item,0);
      return Result;
    };
    this.Clear = function () {
      while (this.FList.FCount > 0) this.Delete(this.GetCount() - 1);
    };
    this.Delete = function (Index) {
      var V = undefined;
      V = this.FList.Get(Index);
      this.FList.Delete(Index);
      if (pas.System.Assigned(V)) this.Notify(V,2);
    };
    this.Insert = function (Index, Item) {
      this.FList.Insert(Index,Item);
      if (pas.System.Assigned(Item)) this.Notify(Item,0);
    };
  });
  rtl.createClass(this,"TPersistent",pas.System.TObject,function () {
    this.AssignError = function (Source) {
      var SourceName = "";
      if (Source !== null) {
        SourceName = Source.$classname}
       else SourceName = "Nil";
      throw pas.SysUtils.EConvertError.$create("Create$1",["Cannot assign a " + SourceName + " to a " + this.$classname + "."]);
    };
    this.DefineProperties = function (Filer) {
      if (Filer === null) return;
    };
    this.AssignTo = function (Dest) {
      Dest.AssignError(this);
    };
    this.Assign = function (Source) {
      if (Source !== null) {
        Source.AssignTo(this)}
       else this.AssignError(null);
    };
  });
  rtl.createClass(this,"TStrings",this.TPersistent,function () {
    this.$init = function () {
      $mod.TPersistent.$init.call(this);
      this.FSpecialCharsInited = false;
      this.FAlwaysQuote = false;
      this.FQuoteChar = "";
      this.FDelimiter = "";
      this.FNameValueSeparator = "";
      this.FUpdateCount = 0;
      this.FLBS = 0;
      this.FSkipLastLineBreak = false;
      this.FLineBreak = "";
    };
    this.DoSetTextStr = function (Value, DoClear) {
      var S = "";
      var P = 0;
      try {
        this.BeginUpdate();
        if (DoClear) this.Clear();
        P = 1;
        while (this.GetNextLinebreak(Value,{get: function () {
            return S;
          }, set: function (v) {
            S = v;
          }},{get: function () {
            return P;
          }, set: function (v) {
            P = v;
          }})) this.Add(S);
      } finally {
        this.EndUpdate();
      };
    };
    this.GetSkipLastLineBreak = function () {
      var Result = false;
      this.CheckSpecialChars();
      Result = this.FSkipLastLineBreak;
      return Result;
    };
    this.ReadData = function (Reader) {
      Reader.ReadListBegin();
      this.BeginUpdate();
      try {
        this.Clear();
        while (!Reader.EndOfList()) this.Add(Reader.ReadString());
      } finally {
        this.EndUpdate();
      };
      Reader.ReadListEnd();
    };
    this.WriteData = function (Writer) {
      var i = 0;
      Writer.WriteListBegin();
      for (var $l = 0, $end = this.GetCount() - 1; $l <= $end; $l++) {
        i = $l;
        Writer.WriteString(this.Get(i));
      };
      Writer.WriteListEnd();
    };
    this.DefineProperties = function (Filer) {
      var HasData = false;
      if (Filer.FAncestor != null) {
        if (Filer.FAncestor.$class.InheritsFrom($mod.TStrings)) {
          HasData = !this.Equals$2(Filer.FAncestor)}
         else HasData = true}
       else HasData = this.GetCount() > 0;
      Filer.DefineProperty("Strings",rtl.createCallback(this,"ReadData"),rtl.createCallback(this,"WriteData"),HasData);
    };
    this.Error = function (Msg, Data) {
      throw $mod.EStringListError.$create("CreateFmt",[Msg,pas.System.VarRecs(18,pas.SysUtils.IntToStr(Data))]);
    };
    this.GetCapacity = function () {
      var Result = 0;
      Result = this.GetCount();
      return Result;
    };
    this.GetObject = function (Index) {
      var Result = null;
      if (Index === 0) ;
      Result = null;
      return Result;
    };
    this.GetTextStr = function () {
      var Result = "";
      var I = 0;
      var S = "";
      var NL = "";
      this.CheckSpecialChars();
      if (this.FLineBreak !== pas.System.sLineBreak) {
        NL = this.FLineBreak}
       else {
        var $tmp = this.FLBS;
        if ($tmp === 0) {
          NL = "\n"}
         else if ($tmp === 1) {
          NL = "\r\n"}
         else if ($tmp === 2) NL = "\r";
      };
      Result = "";
      for (var $l = 0, $end = this.GetCount() - 1; $l <= $end; $l++) {
        I = $l;
        S = this.Get(I);
        Result = Result + S;
        if ((I < (this.GetCount() - 1)) || !this.GetSkipLastLineBreak()) Result = Result + NL;
      };
      return Result;
    };
    this.PutObject = function (Index, AObject) {
      if (Index === 0) return;
      if (AObject === null) return;
    };
    this.SetTextStr = function (Value) {
      this.CheckSpecialChars();
      this.DoSetTextStr(Value,true);
    };
    this.SetUpdateState = function (Updating) {
      if (Updating) ;
    };
    this.DoCompareText = function (s1, s2) {
      var Result = 0;
      Result = pas.SysUtils.CompareText(s1,s2);
      return Result;
    };
    this.CheckSpecialChars = function () {
      if (!this.FSpecialCharsInited) {
        this.FQuoteChar = '"';
        this.FDelimiter = ",";
        this.FNameValueSeparator = "=";
        this.FLBS = pas.System.DefaultTextLineBreakStyle;
        this.FSpecialCharsInited = true;
        this.FLineBreak = pas.System.sLineBreak;
      };
    };
    this.GetNextLinebreak = function (Value, S, P) {
      var Result = false;
      var PPLF = 0;
      var PPCR = 0;
      var PP = 0;
      var PL = 0;
      S.set("");
      Result = false;
      if ((Value.length - P.get()) < 0) return Result;
      PPLF = Value.indexOf("\n",P.get() - 1) + 1;
      PPCR = Value.indexOf("\r",P.get() - 1) + 1;
      PL = 1;
      if ((PPLF > 0) && (PPCR > 0)) {
        if ((PPLF - PPCR) === 1) PL = 2;
        if (PPLF < PPCR) {
          PP = PPLF}
         else PP = PPCR;
      } else if ((PPLF > 0) && (PPCR < 1)) {
        PP = PPLF}
       else if ((PPCR > 0) && (PPLF < 1)) {
        PP = PPCR}
       else PP = Value.length + 1;
      S.set(pas.System.Copy(Value,P.get(),PP - P.get()));
      P.set(PP + PL);
      Result = true;
      return Result;
    };
    this.Create$1 = function () {
      pas.System.TObject.Create.call(this);
      this.FAlwaysQuote = false;
      return this;
    };
    this.Destroy = function () {
      pas.System.TObject.Destroy.call(this);
    };
    this.Add = function (S) {
      var Result = 0;
      Result = this.GetCount();
      this.Insert(this.GetCount(),S);
      return Result;
    };
    this.AddObject = function (S, AObject) {
      var Result = 0;
      Result = this.Add(S);
      this.PutObject(Result,AObject);
      return Result;
    };
    this.AddStrings = function (TheStrings) {
      var Runner = 0;
      for (var $l = 0, $end = TheStrings.GetCount() - 1; $l <= $end; $l++) {
        Runner = $l;
        this.AddObject(TheStrings.Get(Runner),TheStrings.GetObject(Runner));
      };
    };
    this.Assign = function (Source) {
      var S = null;
      if ($mod.TStrings.isPrototypeOf(Source)) {
        S = Source;
        this.BeginUpdate();
        try {
          this.Clear();
          this.FSpecialCharsInited = S.FSpecialCharsInited;
          this.FQuoteChar = S.FQuoteChar;
          this.FDelimiter = S.FDelimiter;
          this.FNameValueSeparator = S.FNameValueSeparator;
          this.FLBS = S.FLBS;
          this.FLineBreak = S.FLineBreak;
          this.AddStrings(S);
        } finally {
          this.EndUpdate();
        };
      } else $mod.TPersistent.Assign.call(this,Source);
    };
    this.BeginUpdate = function () {
      if (this.FUpdateCount === 0) this.SetUpdateState(true);
      this.FUpdateCount += 1;
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) this.FUpdateCount -= 1;
      if (this.FUpdateCount === 0) this.SetUpdateState(false);
    };
    this.Equals$2 = function (TheStrings) {
      var Result = false;
      var Runner = 0;
      var Nr = 0;
      Result = false;
      Nr = this.GetCount();
      if (Nr !== TheStrings.GetCount()) return Result;
      for (var $l = 0, $end = Nr - 1; $l <= $end; $l++) {
        Runner = $l;
        if (this.Get(Runner) !== TheStrings.Get(Runner)) return Result;
      };
      Result = true;
      return Result;
    };
    this.IndexOf = function (S) {
      var Result = 0;
      Result = 0;
      while ((Result < this.GetCount()) && (this.DoCompareText(this.Get(Result),S) !== 0)) Result = Result + 1;
      if (Result === this.GetCount()) Result = -1;
      return Result;
    };
  });
  rtl.recNewT(this,"TStringItem",function () {
    this.FString = "";
    this.FObject = null;
    this.$eq = function (b) {
      return (this.FString === b.FString) && (this.FObject === b.FObject);
    };
    this.$assign = function (s) {
      this.FString = s.FString;
      this.FObject = s.FObject;
      return this;
    };
  });
  this.TStringsSortStyle = {"0": "sslNone", sslNone: 0, "1": "sslUser", sslUser: 1, "2": "sslAuto", sslAuto: 2};
  rtl.createClass(this,"TStringList",this.TStrings,function () {
    this.$init = function () {
      $mod.TStrings.$init.call(this);
      this.FList = [];
      this.FCount = 0;
      this.FOnChange = null;
      this.FOnChanging = null;
      this.FDuplicates = 0;
      this.FCaseSensitive = false;
      this.FOwnsObjects = false;
      this.FSortStyle = 0;
    };
    this.$final = function () {
      this.FList = undefined;
      this.FOnChange = undefined;
      this.FOnChanging = undefined;
      $mod.TStrings.$final.call(this);
    };
    this.GetSorted = function () {
      var Result = false;
      Result = this.FSortStyle in rtl.createSet(1,2);
      return Result;
    };
    this.Grow = function () {
      var NC = 0;
      NC = this.GetCapacity();
      if (NC >= 256) {
        NC = NC + rtl.trunc(NC / 4)}
       else if (NC === 0) {
        NC = 4}
       else NC = NC * 4;
      this.SetCapacity(NC);
    };
    this.InternalClear = function (FromIndex, ClearOnly) {
      var I = 0;
      if (FromIndex < this.FCount) {
        if (this.FOwnsObjects) {
          for (var $l = FromIndex, $end = this.FCount - 1; $l <= $end; $l++) {
            I = $l;
            this.FList[I].FString = "";
            pas.SysUtils.FreeAndNil({p: this.FList[I], get: function () {
                return this.p.FObject;
              }, set: function (v) {
                this.p.FObject = v;
              }});
          };
        } else {
          for (var $l1 = FromIndex, $end1 = this.FCount - 1; $l1 <= $end1; $l1++) {
            I = $l1;
            this.FList[I].FString = "";
          };
        };
        this.FCount = FromIndex;
      };
      if (!ClearOnly) this.SetCapacity(0);
    };
    this.CheckIndex = function (AIndex) {
      if ((AIndex < 0) || (AIndex >= this.FCount)) this.Error(rtl.getResStr(pas.RTLConsts,"SListIndexError"),AIndex);
    };
    this.Changed = function () {
      if (this.FUpdateCount === 0) {
        if (this.FOnChange != null) this.FOnChange(this);
      };
    };
    this.Changing = function () {
      if (this.FUpdateCount === 0) if (this.FOnChanging != null) this.FOnChanging(this);
    };
    this.Get = function (Index) {
      var Result = "";
      this.CheckIndex(Index);
      Result = this.FList[Index].FString;
      return Result;
    };
    this.GetCapacity = function () {
      var Result = 0;
      Result = rtl.length(this.FList);
      return Result;
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FCount;
      return Result;
    };
    this.GetObject = function (Index) {
      var Result = null;
      this.CheckIndex(Index);
      Result = this.FList[Index].FObject;
      return Result;
    };
    this.PutObject = function (Index, AObject) {
      this.CheckIndex(Index);
      this.Changing();
      this.FList[Index].FObject = AObject;
      this.Changed();
    };
    this.SetCapacity = function (NewCapacity) {
      if (NewCapacity < 0) this.Error(rtl.getResStr(pas.RTLConsts,"SListCapacityError"),NewCapacity);
      if (NewCapacity !== this.GetCapacity()) this.FList = rtl.arraySetLength(this.FList,$mod.TStringItem,NewCapacity);
    };
    this.SetUpdateState = function (Updating) {
      if (Updating) {
        this.Changing()}
       else this.Changed();
    };
    this.InsertItem = function (Index, S) {
      this.InsertItem$1(Index,S,null);
    };
    this.InsertItem$1 = function (Index, S, O) {
      var It = $mod.TStringItem.$new();
      this.Changing();
      if (this.FCount === this.GetCapacity()) this.Grow();
      It.FString = S;
      It.FObject = O;
      this.FList.splice(Index,0,It);
      this.FCount += 1;
      this.Changed();
    };
    this.DoCompareText = function (s1, s2) {
      var Result = 0;
      if (this.FCaseSensitive) {
        Result = pas.SysUtils.CompareStr(s1,s2)}
       else Result = pas.SysUtils.CompareText(s1,s2);
      return Result;
    };
    this.Destroy = function () {
      this.InternalClear(0,false);
      $mod.TStrings.Destroy.call(this);
    };
    this.Add = function (S) {
      var Result = 0;
      if (!(this.FSortStyle === 2)) {
        Result = this.FCount}
       else if (this.Find(S,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }})) {
        var $tmp = this.FDuplicates;
        if ($tmp === 0) {
          return Result}
         else if ($tmp === 2) this.Error(rtl.getResStr(pas.RTLConsts,"SDuplicateString"),0);
      };
      this.InsertItem(Result,S);
      return Result;
    };
    this.Clear = function () {
      if (this.FCount === 0) return;
      this.Changing();
      this.InternalClear(0,false);
      this.Changed();
    };
    this.Find = function (S, Index) {
      var Result = false;
      var L = 0;
      var R = 0;
      var I = 0;
      var CompareRes = 0;
      Result = false;
      Index.set(-1);
      if (!this.GetSorted()) throw $mod.EListError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SErrFindNeedsSortedList")]);
      L = 0;
      R = this.GetCount() - 1;
      while (L <= R) {
        I = L + rtl.trunc((R - L) / 2);
        CompareRes = this.DoCompareText(S,this.FList[I].FString);
        if (CompareRes > 0) {
          L = I + 1}
         else {
          R = I - 1;
          if (CompareRes === 0) {
            Result = true;
            if (this.FDuplicates !== 1) L = I;
          };
        };
      };
      Index.set(L);
      return Result;
    };
    this.IndexOf = function (S) {
      var Result = 0;
      if (!this.GetSorted()) {
        Result = $mod.TStrings.IndexOf.call(this,S)}
       else if (!this.Find(S,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }})) Result = -1;
      return Result;
    };
    this.Insert = function (Index, S) {
      if (this.FSortStyle === 2) {
        this.Error(rtl.getResStr(pas.RTLConsts,"SSortedListError"),0)}
       else {
        if ((Index < 0) || (Index > this.FCount)) this.Error(rtl.getResStr(pas.RTLConsts,"SListIndexError"),Index);
        this.InsertItem(Index,S);
      };
    };
  });
  rtl.createClass(this,"TCollectionItem",this.TPersistent,function () {
    this.$init = function () {
      $mod.TPersistent.$init.call(this);
      this.FCollection = null;
      this.FID = 0;
    };
    this.$final = function () {
      this.FCollection = undefined;
      $mod.TPersistent.$final.call(this);
    };
    this.SetCollection = function (Value) {
      if (Value !== this.FCollection) {
        if (this.FCollection != null) this.FCollection.RemoveItem(this);
        if (Value != null) Value.InsertItem(this);
      };
    };
    this.Create$1 = function (ACollection) {
      pas.System.TObject.Create.call(this);
      this.SetCollection(ACollection);
      return this;
    };
    this.Destroy = function () {
      this.SetCollection(null);
      pas.System.TObject.Destroy.call(this);
    };
  });
  this.TCollectionNotification = {"0": "cnAdded", cnAdded: 0, "1": "cnExtracting", cnExtracting: 1, "2": "cnDeleting", cnDeleting: 2};
  rtl.createClass(this,"TCollection",this.TPersistent,function () {
    this.$init = function () {
      $mod.TPersistent.$init.call(this);
      this.FItemClass = null;
      this.FItems = null;
      this.FUpdateCount = 0;
      this.FNextID = 0;
    };
    this.$final = function () {
      this.FItemClass = undefined;
      this.FItems = undefined;
      $mod.TPersistent.$final.call(this);
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FItems.FCount;
      return Result;
    };
    this.InsertItem = function (Item) {
      if (!this.FItemClass.isPrototypeOf(Item)) return;
      this.FItems.Add(Item);
      Item.FCollection = this;
      Item.FID = this.FNextID;
      this.FNextID += 1;
      this.SetItemName(Item);
      this.Notify(Item,0);
      this.Changed();
    };
    this.RemoveItem = function (Item) {
      var I = 0;
      this.Notify(Item,1);
      I = this.FItems.IndexOfItem(Item,1);
      if (I !== -1) this.FItems.Delete(I);
      Item.FCollection = null;
      this.Changed();
    };
    this.DoClear = function () {
      var Item = null;
      while (this.FItems.FCount > 0) {
        Item = rtl.getObject(this.FItems.Last());
        if (Item != null) Item.$destroy("Destroy");
      };
    };
    this.Changed = function () {
      if (this.FUpdateCount === 0) this.Update(null);
    };
    this.GetItem = function (Index) {
      var Result = null;
      Result = rtl.getObject(this.FItems.Get(Index));
      return Result;
    };
    this.SetItemName = function (Item) {
      if (Item === null) ;
    };
    this.Update = function (Item) {
      if (Item === null) ;
    };
    this.Notify = function (Item, Action) {
      if (Item === null) ;
      if (Action === 0) ;
    };
    this.Destroy = function () {
      this.FUpdateCount = 1;
      try {
        this.DoClear();
      } finally {
        this.FUpdateCount = 0;
      };
      if (this.FItems != null) this.FItems.$destroy("Destroy");
      pas.System.TObject.Destroy.call(this);
    };
    this.Add = function () {
      var Result = null;
      Result = this.FItemClass.$create("Create$1",[this]);
      return Result;
    };
    this.Assign = function (Source) {
      var I = 0;
      if ($mod.TCollection.isPrototypeOf(Source)) {
        this.Clear();
        for (var $l = 0, $end = Source.GetCount() - 1; $l <= $end; $l++) {
          I = $l;
          this.Add().Assign(Source.GetItem(I));
        };
        return;
      } else $mod.TPersistent.Assign.call(this,Source);
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.Clear = function () {
      if (this.FItems.FCount === 0) return;
      this.BeginUpdate();
      try {
        this.DoClear();
      } finally {
        this.EndUpdate();
      };
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) this.FUpdateCount -= 1;
      if (this.FUpdateCount === 0) this.Changed();
    };
  });
  this.TOperation = {"0": "opInsert", opInsert: 0, "1": "opRemove", opRemove: 1};
  this.TComponentStateItem = {"0": "csLoading", csLoading: 0, "1": "csReading", csReading: 1, "2": "csWriting", csWriting: 2, "3": "csDestroying", csDestroying: 3, "4": "csDesigning", csDesigning: 4, "5": "csAncestor", csAncestor: 5, "6": "csUpdating", csUpdating: 6, "7": "csFixups", csFixups: 7, "8": "csFreeNotification", csFreeNotification: 8, "9": "csInline", csInline: 9, "10": "csDesignInstance", csDesignInstance: 10};
  this.TComponentStyleItem = {"0": "csInheritable", csInheritable: 0, "1": "csCheckPropAvail", csCheckPropAvail: 1, "2": "csSubComponent", csSubComponent: 2, "3": "csTransient", csTransient: 3};
  rtl.createClass(this,"TComponent",this.TPersistent,function () {
    this.$init = function () {
      $mod.TPersistent.$init.call(this);
      this.FOwner = null;
      this.FName = "";
      this.FTag = 0;
      this.FComponents = null;
      this.FFreeNotifies = null;
      this.FDesignInfo = 0;
      this.FComponentState = {};
      this.FComponentStyle = {};
    };
    this.$final = function () {
      this.FOwner = undefined;
      this.FComponents = undefined;
      this.FFreeNotifies = undefined;
      this.FComponentState = undefined;
      this.FComponentStyle = undefined;
      $mod.TPersistent.$final.call(this);
    };
    this.GetComponent = function (AIndex) {
      var Result = null;
      if (!(this.FComponents != null)) {
        Result = null}
       else Result = rtl.getObject(this.FComponents.Get(AIndex));
      return Result;
    };
    this.GetComponentCount = function () {
      var Result = 0;
      if (!(this.FComponents != null)) {
        Result = 0}
       else Result = this.FComponents.FCount;
      return Result;
    };
    this.Insert = function (AComponent) {
      if (!(this.FComponents != null)) this.FComponents = $mod.TFPList.$create("Create");
      this.FComponents.Add(AComponent);
      AComponent.FOwner = this;
    };
    this.ReadLeft = function (AReader) {
      this.FDesignInfo = (this.FDesignInfo & 0xffff0000) | (AReader.ReadInteger() & 0xffff);
    };
    this.ReadTop = function (AReader) {
      this.FDesignInfo = ((AReader.ReadInteger() & 0xffff) << 16) | (this.FDesignInfo & 0xffff);
    };
    this.Remove = function (AComponent) {
      AComponent.FOwner = null;
      if (this.FComponents != null) {
        this.FComponents.Remove(AComponent);
        if (this.FComponents.FCount === 0) {
          this.FComponents.$destroy("Destroy");
          this.FComponents = null;
        };
      };
    };
    this.RemoveNotification = function (AComponent) {
      if (this.FFreeNotifies !== null) {
        this.FFreeNotifies.Remove(AComponent);
        if (this.FFreeNotifies.FCount === 0) {
          this.FFreeNotifies.$destroy("Destroy");
          this.FFreeNotifies = null;
          this.FComponentState = rtl.excludeSet(this.FComponentState,8);
        };
      };
    };
    this.SetReference = function (Enable) {
      var aField = null;
      var aValue = null;
      var aOwner = null;
      if (this.FName === "") return;
      if (this.FOwner != null) {
        aOwner = this.FOwner;
        aField = this.FOwner.$class.FieldAddress(this.FName);
        if (aField != null) {
          if (Enable) {
            aValue = this}
           else aValue = null;
          aOwner["" + aField["name"]] = aValue;
        };
      };
    };
    this.WriteLeft = function (AWriter) {
      AWriter.WriteInteger(this.FDesignInfo & 0xffff);
    };
    this.WriteTop = function (AWriter) {
      AWriter.WriteInteger((this.FDesignInfo >>> 16) & 0xffff);
    };
    this.ChangeName = function (NewName) {
      this.FName = NewName;
    };
    this.DefineProperties = function (Filer) {
      var Temp = 0;
      var Ancestor = null;
      Ancestor = Filer.FAncestor;
      if (Ancestor != null) {
        Temp = Ancestor.FDesignInfo}
       else Temp = 0;
      Filer.DefineProperty("Left",rtl.createCallback(this,"ReadLeft"),rtl.createCallback(this,"WriteLeft"),(this.FDesignInfo & 0xffff) !== (Temp & 0xffff));
      Filer.DefineProperty("Top",rtl.createCallback(this,"ReadTop"),rtl.createCallback(this,"WriteTop"),(this.FDesignInfo & 0xffff0000) !== (Temp & 0xffff0000));
    };
    this.GetChildOwner = function () {
      var Result = null;
      Result = null;
      return Result;
    };
    this.GetChildParent = function () {
      var Result = null;
      Result = this;
      return Result;
    };
    this.Loaded = function () {
      this.FComponentState = rtl.excludeSet(this.FComponentState,0);
    };
    this.Notification = function (AComponent, Operation) {
      var C = 0;
      if (Operation === 1) this.RemoveFreeNotification(AComponent);
      if (!(this.FComponents != null)) return;
      C = this.FComponents.FCount - 1;
      while (C >= 0) {
        rtl.getObject(this.FComponents.Get(C)).Notification(AComponent,Operation);
        C -= 1;
        if (C >= this.FComponents.FCount) C = this.FComponents.FCount - 1;
      };
    };
    this.ReadState = function (Reader) {
      Reader.ReadData(this);
    };
    this.SetDesigning = function (Value, SetChildren) {
      var Runner = 0;
      if (Value) {
        this.FComponentState = rtl.includeSet(this.FComponentState,4)}
       else this.FComponentState = rtl.excludeSet(this.FComponentState,4);
      if ((this.FComponents != null) && SetChildren) for (var $l = 0, $end = this.FComponents.FCount - 1; $l <= $end; $l++) {
        Runner = $l;
        rtl.getObject(this.FComponents.Get(Runner)).SetDesigning(Value,true);
      };
    };
    this.SetName = function (NewName) {
      if (this.FName === NewName) return;
      if ((NewName !== "") && !pas.SysUtils.IsValidIdent(NewName,false,false)) throw $mod.EComponentError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SInvalidName"),pas.System.VarRecs(18,NewName)]);
      if (this.FOwner != null) {
        this.FOwner.ValidateRename(this,this.FName,NewName)}
       else this.ValidateRename(null,this.FName,NewName);
      this.SetReference(false);
      this.ChangeName(NewName);
      this.SetReference(true);
    };
    this.SetChildOrder = function (Child, Order) {
      if (Child === null) ;
      if (Order === 0) ;
    };
    this.SetParentComponent = function (Value) {
      if (Value === null) ;
    };
    this.ValidateRename = function (AComponent, CurName, NewName) {
      if ((AComponent !== null) && (pas.SysUtils.CompareText(CurName,NewName) !== 0) && (AComponent.FOwner === this) && (this.FindComponent(NewName) !== null)) throw $mod.EComponentError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SDuplicateName"),pas.System.VarRecs(18,NewName)]);
      if ((4 in this.FComponentState) && (this.FOwner !== null)) this.FOwner.ValidateRename(AComponent,CurName,NewName);
    };
    this.ValidateContainer = function (AComponent) {
      AComponent.ValidateInsert(this);
    };
    this.ValidateInsert = function (AComponent) {
      if (AComponent === null) ;
    };
    this._AddRef = function () {
      var Result = 0;
      Result = -1;
      return Result;
    };
    this._Release = function () {
      var Result = 0;
      Result = -1;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      this.FComponentStyle = rtl.createSet(0);
      if (AOwner != null) AOwner.InsertComponent(this);
      return this;
    };
    this.Destroy = function () {
      var I = 0;
      var C = null;
      this.Destroying();
      if (this.FFreeNotifies != null) {
        I = this.FFreeNotifies.FCount - 1;
        while (I >= 0) {
          C = rtl.getObject(this.FFreeNotifies.Get(I));
          this.FFreeNotifies.Delete(I);
          C.Notification(this,1);
          if (this.FFreeNotifies === null) {
            I = 0}
           else if (I > this.FFreeNotifies.FCount) I = this.FFreeNotifies.FCount;
          I -= 1;
        };
        pas.SysUtils.FreeAndNil({p: this, get: function () {
            return this.p.FFreeNotifies;
          }, set: function (v) {
            this.p.FFreeNotifies = v;
          }});
      };
      this.DestroyComponents();
      if (this.FOwner !== null) this.FOwner.RemoveComponent(this);
      pas.System.TObject.Destroy.call(this);
    };
    this.BeforeDestruction = function () {
      if (!(3 in this.FComponentState)) this.Destroying();
    };
    this.DestroyComponents = function () {
      var acomponent = null;
      while (this.FComponents != null) {
        acomponent = rtl.getObject(this.FComponents.Last());
        this.Remove(acomponent);
        acomponent.$destroy("Destroy");
      };
    };
    this.Destroying = function () {
      var Runner = 0;
      if (3 in this.FComponentState) return;
      this.FComponentState = rtl.includeSet(this.FComponentState,3);
      if (this.FComponents != null) for (var $l = 0, $end = this.FComponents.FCount - 1; $l <= $end; $l++) {
        Runner = $l;
        rtl.getObject(this.FComponents.Get(Runner)).Destroying();
      };
    };
    this.QueryInterface = function (IID, Obj) {
      var Result = 0;
      if (this.GetInterface(IID,Obj)) {
        Result = 0}
       else Result = -2147467262;
      return Result;
    };
    this.FindComponent = function (AName) {
      var Result = null;
      var I = 0;
      Result = null;
      if ((AName === "") || !(this.FComponents != null)) return Result;
      for (var $l = 0, $end = this.FComponents.FCount - 1; $l <= $end; $l++) {
        I = $l;
        if (pas.SysUtils.CompareText(rtl.getObject(this.FComponents.Get(I)).FName,AName) === 0) {
          Result = rtl.getObject(this.FComponents.Get(I));
          return Result;
        };
      };
      return Result;
    };
    this.RemoveFreeNotification = function (AComponent) {
      this.RemoveNotification(AComponent);
      AComponent.RemoveNotification(this);
    };
    this.GetParentComponent = function () {
      var Result = null;
      Result = null;
      return Result;
    };
    this.InsertComponent = function (AComponent) {
      AComponent.ValidateContainer(this);
      this.ValidateRename(AComponent,"",AComponent.FName);
      this.Insert(AComponent);
      if (4 in this.FComponentState) AComponent.SetDesigning(true,true);
      this.Notification(AComponent,0);
    };
    this.RemoveComponent = function (AComponent) {
      this.Notification(AComponent,1);
      this.Remove(AComponent);
      AComponent.SetDesigning(false,true);
      this.ValidateRename(AComponent,AComponent.FName,"");
    };
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Name",6,rtl.string,"FName","SetName");
    $r.addProperty("Tag",0,rtl.nativeint,"FTag","FTag",{Default: 0});
  });
  this.$rtti.$ClassRef("TComponentClass",{instancetype: this.$rtti["TComponent"]});
  this.TSeekOrigin = {"0": "soBeginning", soBeginning: 0, "1": "soCurrent", soCurrent: 1, "2": "soEnd", soEnd: 2};
  rtl.createClass(this,"TStream",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FEndian = 0;
    };
    this.MakeInt = function (B, aSize, Signed) {
      var Result = 0;
      var Mem = null;
      var A = null;
      var D = null;
      var isLittle = false;
      isLittle = this.FEndian === 0;
      Mem = new ArrayBuffer(rtl.length(B));
      A = new Uint8Array(Mem);
      A.set(B);
      D = new DataView(Mem);
      if (Signed) {
        var $tmp = aSize;
        if ($tmp === 1) {
          Result = D.getInt8(0)}
         else if ($tmp === 2) {
          Result = D.getInt16(0,isLittle)}
         else if ($tmp === 4) {
          Result = D.getInt32(0,isLittle)}
         else if ($tmp === 8) Result = Math.round(D.getFloat64(0,isLittle));
      } else {
        var $tmp1 = aSize;
        if ($tmp1 === 1) {
          Result = D.getUint8(0)}
         else if ($tmp1 === 2) {
          Result = D.getUint16(0,isLittle)}
         else if ($tmp1 === 4) {
          Result = D.getUint32(0,isLittle)}
         else if ($tmp1 === 8) Result = Math.round(D.getFloat64(0,isLittle));
      };
      return Result;
    };
    this.MakeBytes = function (B, aSize, Signed) {
      var Result = [];
      var Mem = null;
      var A = null;
      var D = null;
      var isLittle = false;
      isLittle = this.FEndian === 0;
      Mem = new ArrayBuffer(aSize);
      D = new DataView(Mem);
      if (Signed) {
        var $tmp = aSize;
        if ($tmp === 1) {
          D.setInt8(0,B)}
         else if ($tmp === 2) {
          D.setInt16(0,B,isLittle)}
         else if ($tmp === 4) {
          D.setInt32(0,B,isLittle)}
         else if ($tmp === 8) D.setFloat64(0,B,isLittle);
      } else {
        var $tmp1 = aSize;
        if ($tmp1 === 1) {
          D.setUint8(0,B)}
         else if ($tmp1 === 2) {
          D.setUint16(0,B,isLittle)}
         else if ($tmp1 === 4) {
          D.setUint32(0,B,isLittle)}
         else if ($tmp1 === 8) D.setFloat64(0,B,isLittle);
      };
      Result = rtl.arraySetLength(Result,0,aSize);
      A = new Uint8Array(Mem);
      Result = $mod.TMemoryStream.MemoryToBytes$1(A);
      return Result;
    };
    this.GetPosition = function () {
      var Result = 0;
      Result = this.Seek(0,1);
      return Result;
    };
    this.SetPosition = function (Pos) {
      this.Seek(Pos,0);
    };
    this.GetSize = function () {
      var Result = 0;
      var p = 0;
      p = this.Seek(0,1);
      Result = this.Seek(0,2);
      this.Seek(p,0);
      return Result;
    };
    this.ReadMaxSizeData = function (Buffer, aSize, aCount) {
      var Result = 0;
      var CP = 0;
      if (aCount <= aSize) {
        Result = this.Read({get: function () {
            return Buffer;
          }, set: function (v) {
            Buffer = v;
          }},aCount)}
       else {
        Result = this.Read({get: function () {
            return Buffer;
          }, set: function (v) {
            Buffer = v;
          }},aSize);
        CP = this.GetPosition();
        Result = (Result + this.Seek(aCount - aSize,1)) - CP;
      };
      return Result;
    };
    this.WriteMaxSizeData = function (Buffer, aSize, aCount) {
      var Result = 0;
      var CP = 0;
      if (aCount <= aSize) {
        Result = this.Write(Buffer,aCount)}
       else {
        Result = this.Write(Buffer,aSize);
        CP = this.GetPosition();
        Result = (Result + this.Seek(aCount - aSize,1)) - CP;
      };
      return Result;
    };
    this.Read = function (Buffer, Count) {
      var Result = 0;
      Result = this.Read$1(rtl.arrayRef(Buffer.get()),0,Count);
      return Result;
    };
    this.Write = function (Buffer, Count) {
      var Result = 0;
      Result = this.Write$1(Buffer,0,Count);
      return Result;
    };
    this.ReadData$3 = function (Buffer) {
      var Result = 0;
      Result = this.ReadData$4(Buffer,2);
      return Result;
    };
    this.ReadData$4 = function (Buffer, Count) {
      var Result = 0;
      var W = 0;
      Result = this.ReadData$12({get: function () {
          return W;
        }, set: function (v) {
          W = v;
        }},Count);
      if (Result === 2) Buffer.set(String.fromCharCode(W));
      return Result;
    };
    this.ReadData$6 = function (Buffer, Count) {
      var Result = 0;
      var B = [];
      B = rtl.arraySetLength(B,0,Count);
      Result = this.ReadMaxSizeData(rtl.arrayRef(B),1,Count);
      if (Result >= 1) Buffer.set(this.MakeInt(rtl.arrayRef(B),1,true));
      return Result;
    };
    this.ReadData$8 = function (Buffer, Count) {
      var Result = 0;
      var B = [];
      B = rtl.arraySetLength(B,0,Count);
      Result = this.ReadMaxSizeData(rtl.arrayRef(B),1,Count);
      if (Result >= 1) Buffer.set(this.MakeInt(rtl.arrayRef(B),1,false));
      return Result;
    };
    this.ReadData$10 = function (Buffer, Count) {
      var Result = 0;
      var B = [];
      B = rtl.arraySetLength(B,0,Count);
      Result = this.ReadMaxSizeData(rtl.arrayRef(B),2,Count);
      if (Result >= 2) Buffer.set(this.MakeInt(rtl.arrayRef(B),2,true));
      return Result;
    };
    this.ReadData$12 = function (Buffer, Count) {
      var Result = 0;
      var B = [];
      B = rtl.arraySetLength(B,0,Count);
      Result = this.ReadMaxSizeData(rtl.arrayRef(B),2,Count);
      if (Result >= 2) Buffer.set(this.MakeInt(rtl.arrayRef(B),2,false));
      return Result;
    };
    this.ReadData$14 = function (Buffer, Count) {
      var Result = 0;
      var B = [];
      B = rtl.arraySetLength(B,0,Count);
      Result = this.ReadMaxSizeData(rtl.arrayRef(B),4,Count);
      if (Result >= 4) Buffer.set(this.MakeInt(rtl.arrayRef(B),4,true));
      return Result;
    };
    this.ReadData$16 = function (Buffer, Count) {
      var Result = 0;
      var B = [];
      B = rtl.arraySetLength(B,0,Count);
      Result = this.ReadMaxSizeData(rtl.arrayRef(B),4,Count);
      if (Result >= 4) Buffer.set(this.MakeInt(rtl.arrayRef(B),4,false));
      return Result;
    };
    this.ReadData$18 = function (Buffer, Count) {
      var Result = 0;
      var B = [];
      B = rtl.arraySetLength(B,0,Count);
      Result = this.ReadMaxSizeData(rtl.arrayRef(B),8,8);
      if (Result >= 8) Buffer.set(this.MakeInt(rtl.arrayRef(B),8,true));
      return Result;
    };
    this.ReadData$22 = function (Buffer, Count) {
      var Result = 0;
      var B = [];
      var Mem = null;
      var A = null;
      var D = null;
      B = rtl.arraySetLength(B,0,Count);
      Result = this.ReadMaxSizeData(rtl.arrayRef(B),8,Count);
      if (Result >= 8) {
        Mem = new ArrayBuffer(8);
        A = new Uint8Array(Mem);
        A.set(B);
        D = new DataView(Mem);
        Buffer.set(D.getFloat64(0));
      };
      return Result;
    };
    this.ReadBufferData$2 = function (Buffer) {
      this.ReadBufferData$3(Buffer,2);
    };
    this.ReadBufferData$3 = function (Buffer, Count) {
      if (this.ReadData$4(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SReadError")]);
    };
    this.ReadBufferData$4 = function (Buffer) {
      this.ReadBufferData$5(Buffer,1);
    };
    this.ReadBufferData$5 = function (Buffer, Count) {
      if (this.ReadData$6(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SReadError")]);
    };
    this.ReadBufferData$6 = function (Buffer) {
      this.ReadBufferData$7(Buffer,1);
    };
    this.ReadBufferData$7 = function (Buffer, Count) {
      if (this.ReadData$8(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SReadError")]);
    };
    this.ReadBufferData$8 = function (Buffer) {
      this.ReadBufferData$9(Buffer,2);
    };
    this.ReadBufferData$9 = function (Buffer, Count) {
      if (this.ReadData$10(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SReadError")]);
    };
    this.ReadBufferData$12 = function (Buffer) {
      this.ReadBufferData$13(Buffer,4);
    };
    this.ReadBufferData$13 = function (Buffer, Count) {
      if (this.ReadData$14(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SReadError")]);
    };
    this.ReadBufferData$14 = function (Buffer) {
      this.ReadBufferData$15(Buffer,4);
    };
    this.ReadBufferData$15 = function (Buffer, Count) {
      if (this.ReadData$16(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SReadError")]);
    };
    this.ReadBufferData$16 = function (Buffer) {
      this.ReadBufferData$17(Buffer,8);
    };
    this.ReadBufferData$17 = function (Buffer, Count) {
      if (this.ReadData$18(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SReadError")]);
    };
    this.ReadBufferData$20 = function (Buffer) {
      this.ReadBufferData$21(Buffer,8);
    };
    this.ReadBufferData$21 = function (Buffer, Count) {
      if (this.ReadData$22(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SReadError")]);
    };
    this.WriteBuffer = function (Buffer, Count) {
      this.WriteBuffer$1(Buffer,0,Count);
    };
    this.WriteBuffer$1 = function (Buffer, Offset, Count) {
      if (this.Write$1(Buffer,Offset,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SWriteError")]);
    };
    this.WriteData$4 = function (Buffer, Count) {
      var Result = 0;
      var U = 0;
      U = Buffer.charCodeAt();
      Result = this.WriteData$12(U,Count);
      return Result;
    };
    this.WriteData$8 = function (Buffer, Count) {
      var Result = 0;
      Result = this.WriteMaxSizeData(this.MakeBytes(Buffer,1,false),1,Count);
      return Result;
    };
    this.WriteData$12 = function (Buffer, Count) {
      var Result = 0;
      Result = this.WriteMaxSizeData(this.MakeBytes(Buffer,2,true),2,Count);
      return Result;
    };
    this.WriteData$16 = function (Buffer, Count) {
      var Result = 0;
      Result = this.WriteMaxSizeData(this.MakeBytes(Buffer,4,false),4,Count);
      return Result;
    };
    this.WriteData$18 = function (Buffer, Count) {
      var Result = 0;
      Result = this.WriteMaxSizeData(this.MakeBytes(Buffer,8,true),8,Count);
      return Result;
    };
    this.WriteData$22 = function (Buffer, Count) {
      var Result = 0;
      var Mem = null;
      var A = null;
      var D = null;
      var B = [];
      var I = 0;
      Mem = new ArrayBuffer(8);
      D = new DataView(Mem);
      D.setFloat64(0,Buffer);
      B = rtl.arraySetLength(B,0,8);
      A = new Uint8Array(Mem);
      for (I = 0; I <= 7; I++) B[I] = A[I];
      Result = this.WriteMaxSizeData(B,8,Count);
      return Result;
    };
    this.WriteBufferData$4 = function (Buffer) {
      this.WriteBufferData$5(Buffer,2);
    };
    this.WriteBufferData$5 = function (Buffer, Count) {
      if (this.WriteData$4(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SWriteError")]);
    };
    this.WriteBufferData$9 = function (Buffer, Count) {
      if (this.WriteData$8(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SWriteError")]);
    };
    this.WriteBufferData$12 = function (Buffer) {
      this.WriteBufferData$13(Buffer,2);
    };
    this.WriteBufferData$13 = function (Buffer, Count) {
      if (this.WriteData$12(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SWriteError")]);
    };
    this.WriteBufferData$14 = function (Buffer) {
      this.WriteBufferData$15(Buffer,4);
    };
    this.WriteBufferData$15 = function (Buffer, Count) {
      if (this.WriteData$16(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SWriteError")]);
    };
    this.WriteBufferData$16 = function (Buffer) {
      this.WriteBufferData$17(Buffer,8);
    };
    this.WriteBufferData$17 = function (Buffer, Count) {
      if (this.WriteData$18(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SWriteError")]);
    };
    this.WriteBufferData$20 = function (Buffer) {
      this.WriteBufferData$21(Buffer,8);
    };
    this.WriteBufferData$21 = function (Buffer, Count) {
      if (this.WriteData$22(Buffer,Count) !== Count) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SWriteError")]);
    };
    this.WriteByte = function (b) {
      this.WriteBufferData$9(b,1);
    };
  });
  rtl.createClass(this,"TCustomMemoryStream",this.TStream,function () {
    this.$init = function () {
      $mod.TStream.$init.call(this);
      this.FMemory = null;
      this.FDataView = null;
      this.FDataArray = null;
      this.FSize = 0;
      this.FPosition = 0;
      this.FSizeBoundsSeek = false;
    };
    this.$final = function () {
      this.FMemory = undefined;
      this.FDataView = undefined;
      this.FDataArray = undefined;
      $mod.TStream.$final.call(this);
    };
    this.GetDataArray = function () {
      var Result = null;
      if (this.FDataArray === null) this.FDataArray = new Uint8Array(this.FMemory);
      Result = this.FDataArray;
      return Result;
    };
    this.GetDataView = function () {
      var Result = null;
      if (this.FDataView === null) this.FDataView = new DataView(this.FMemory);
      Result = this.FDataView;
      return Result;
    };
    this.GetSize = function () {
      var Result = 0;
      Result = this.FSize;
      return Result;
    };
    this.GetPosition = function () {
      var Result = 0;
      Result = this.FPosition;
      return Result;
    };
    this.SetPointer = function (Ptr, ASize) {
      this.FMemory = Ptr;
      this.FSize = ASize;
      this.FDataView = null;
      this.FDataArray = null;
    };
    this.MemoryToBytes = function (Mem) {
      var Result = [];
      Result = this.MemoryToBytes$1(new Uint8Array(Mem));
      return Result;
    };
    this.MemoryToBytes$1 = function (Mem) {
      var Result = [];
      var I = 0;
      for (var $l = 0, $end = Mem.length - 1; $l <= $end; $l++) {
        I = $l;
        Result[I] = Mem[I];
      };
      return Result;
    };
    this.Read$1 = function (Buffer, Offset, Count) {
      var Result = 0;
      var I = 0;
      var Src = 0;
      var Dest = 0;
      Result = 0;
      if ((this.FSize > 0) && (this.FPosition < this.FSize) && (this.FPosition >= 0)) {
        Result = Count;
        if (Result > (this.FSize - this.FPosition)) Result = this.FSize - this.FPosition;
        Src = this.FPosition;
        Dest = Offset;
        I = 0;
        while (I < Result) {
          Buffer[Dest] = this.GetDataView().getUint8(Src);
          Src += 1;
          Dest += 1;
          I += 1;
        };
        this.FPosition = this.FPosition + Result;
      };
      return Result;
    };
    this.Seek = function (Offset, Origin) {
      var Result = 0;
      var $tmp = Origin;
      if ($tmp === 0) {
        this.FPosition = Offset}
       else if ($tmp === 2) {
        this.FPosition = this.FSize + Offset}
       else if ($tmp === 1) this.FPosition = this.FPosition + Offset;
      if (this.FSizeBoundsSeek && (this.FPosition > this.FSize)) this.FPosition = this.FSize;
      Result = this.FPosition;
      return Result;
    };
  });
  rtl.createClass(this,"TMemoryStream",this.TCustomMemoryStream,function () {
    this.$init = function () {
      $mod.TCustomMemoryStream.$init.call(this);
      this.FCapacity = 0;
    };
    this.SetCapacity = function (NewCapacity) {
      this.SetPointer(this.Realloc({get: function () {
          return NewCapacity;
        }, set: function (v) {
          NewCapacity = v;
        }}),this.FSize);
      this.FCapacity = NewCapacity;
    };
    this.Realloc = function (NewCapacity) {
      var Result = null;
      var GC = 0;
      var DestView = null;
      if (NewCapacity.get() < 0) {
        NewCapacity.set(0)}
       else {
        GC = this.FCapacity + rtl.trunc(this.FCapacity / 4);
        if ((NewCapacity.get() > this.FCapacity) && (NewCapacity.get() < GC)) NewCapacity.set(GC);
        NewCapacity.set((NewCapacity.get() + (4096 - 1)) & ~(4096 - 1));
      };
      if (NewCapacity.get() === this.FCapacity) {
        Result = this.FMemory}
       else if (NewCapacity.get() === 0) {
        Result = null}
       else {
        Result = new ArrayBuffer(NewCapacity.get());
        if (Result === null) throw $mod.EStreamError.$create("Create$1",[rtl.getResStr($mod,"SMemoryStreamError")]);
        DestView = new Uint8Array(Result);
        DestView.set(this.GetDataArray());
      };
      return Result;
    };
    this.Destroy = function () {
      this.Clear();
      pas.System.TObject.Destroy.call(this);
    };
    this.Clear = function () {
      this.FSize = 0;
      this.FPosition = 0;
      this.SetCapacity(0);
    };
    this.Write$1 = function (Buffer, Offset, Count) {
      var Result = 0;
      var NewPos = 0;
      if ((Count === 0) || (this.FPosition < 0)) return 0;
      NewPos = this.FPosition + Count;
      if (NewPos > this.FSize) {
        if (NewPos > this.FCapacity) this.SetCapacity(NewPos);
        this.FSize = NewPos;
      };
      this.GetDataArray().set(rtl.arrayCopy(0,Buffer,Offset,Count),this.FPosition);
      this.FPosition = NewPos;
      Result = Count;
      return Result;
    };
  });
  rtl.createClass(this,"TBytesStream",this.TMemoryStream,function () {
    this.GetBytes = function () {
      var Result = [];
      Result = $mod.TMemoryStream.MemoryToBytes(this.FMemory);
      return Result;
    };
  });
  rtl.createClass(this,"TStringStream",this.TMemoryStream,function () {
    this.Create$2 = function (aString) {
      var Len = 0;
      pas.System.TObject.Create.call(this);
      Len = aString.length;
      this.SetPointer($mod.StringToBuffer(aString,Len),Len * 2);
      this.FCapacity = Len * 2;
      return this;
    };
  });
  this.TFilerFlag = {"0": "ffInherited", ffInherited: 0, "1": "ffChildPos", ffChildPos: 1, "2": "ffInline", ffInline: 2};
  rtl.createClass(this,"TFiler",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FRoot = null;
      this.FLookupRoot = null;
      this.FAncestor = null;
    };
    this.$final = function () {
      this.FRoot = undefined;
      this.FLookupRoot = undefined;
      this.FAncestor = undefined;
      pas.System.TObject.$final.call(this);
    };
  });
  this.TValueType = {"0": "vaNull", vaNull: 0, "1": "vaList", vaList: 1, "2": "vaInt8", vaInt8: 2, "3": "vaInt16", vaInt16: 3, "4": "vaInt32", vaInt32: 4, "5": "vaDouble", vaDouble: 5, "6": "vaString", vaString: 6, "7": "vaIdent", vaIdent: 7, "8": "vaFalse", vaFalse: 8, "9": "vaTrue", vaTrue: 9, "10": "vaBinary", vaBinary: 10, "11": "vaSet", vaSet: 11, "12": "vaNil", vaNil: 12, "13": "vaCollection", vaCollection: 13, "14": "vaCurrency", vaCurrency: 14, "15": "vaDate", vaDate: 15, "16": "vaNativeInt", vaNativeInt: 16};
  rtl.createClass(this,"TAbstractObjectReader",pas.System.TObject,function () {
  });
  rtl.createClass(this,"TBinaryObjectReader",this.TAbstractObjectReader,function () {
    this.$init = function () {
      $mod.TAbstractObjectReader.$init.call(this);
      this.FStream = null;
    };
    this.$final = function () {
      this.FStream = undefined;
      $mod.TAbstractObjectReader.$final.call(this);
    };
    this.ReadDWord = function () {
      var Result = 0;
      this.FStream.ReadBufferData$14({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }});
      return Result;
    };
    this.SkipProperty = function () {
      this.ReadStr();
      this.SkipValue();
    };
    this.SkipSetBody = function () {
      while (this.ReadStr().length > 0) {
      };
    };
    this.Create$1 = function (Stream) {
      pas.System.TObject.Create.call(this);
      if (Stream === null) throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SEmptyStreamIllegalReader")]);
      this.FStream = Stream;
      return this;
    };
    this.NextValue = function () {
      var Result = 0;
      Result = this.ReadValue();
      this.FStream.Seek(-1,1);
      return Result;
    };
    this.ReadValue = function () {
      var Result = 0;
      var b = 0;
      this.FStream.ReadBufferData$6({get: function () {
          return b;
        }, set: function (v) {
          b = v;
        }});
      Result = b;
      return Result;
    };
    this.BeginRootComponent = function () {
      this.ReadSignature();
    };
    this.BeginComponent = function (Flags, AChildPos, CompClassName, CompName) {
      var Prefix = 0;
      var ValueType = 0;
      Flags.set({});
      if ((this.NextValue() & 0xf0) === 0xf0) {
        Prefix = this.ReadValue();
        Flags.set({});
        if ((Prefix & 0x1) !== 0) Flags.set(rtl.includeSet(Flags.get(),0));
        if ((Prefix & 0x2) !== 0) Flags.set(rtl.includeSet(Flags.get(),1));
        if ((Prefix & 0x4) !== 0) Flags.set(rtl.includeSet(Flags.get(),2));
        if (1 in Flags.get()) {
          ValueType = this.ReadValue();
          var $tmp = ValueType;
          if ($tmp === 2) {
            AChildPos.set(this.ReadInt8())}
           else if ($tmp === 3) {
            AChildPos.set(this.ReadInt16())}
           else if ($tmp === 4) {
            AChildPos.set(this.ReadInt32())}
           else if ($tmp === 16) {
            AChildPos.set(this.ReadNativeInt())}
           else {
            throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyValue")]);
          };
        };
      };
      CompClassName.set(this.ReadStr());
      CompName.set(this.ReadStr());
    };
    this.BeginProperty = function () {
      var Result = "";
      Result = this.ReadStr();
      return Result;
    };
    this.Read = function (Buffer, Count) {
      this.FStream.Read(Buffer,Count);
    };
    this.ReadFloat = function () {
      var Result = 0.0;
      this.FStream.ReadBufferData$20({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }});
      return Result;
    };
    this.ReadCurrency = function () {
      var Result = 0;
      Result = rtl.trunc(this.ReadFloat() * 10000);
      return Result;
    };
    this.ReadIdent = function (ValueType) {
      var Result = "";
      var i = 0;
      var c = "";
      var $tmp = ValueType;
      if ($tmp === 7) {
        this.FStream.ReadBufferData$6({get: function () {
            return i;
          }, set: function (v) {
            i = v;
          }});
        Result = rtl.strSetLength(Result,i);
        for (var $l = 1, $end = Result.length; $l <= $end; $l++) {
          i = $l;
          this.FStream.ReadBufferData$2({get: function () {
              return c;
            }, set: function (v) {
              c = v;
            }});
          Result = rtl.setCharAt(Result,i - 1,c);
        };
      } else if ($tmp === 12) {
        Result = "nil"}
       else if ($tmp === 8) {
        Result = "False"}
       else if ($tmp === 9) {
        Result = "True"}
       else if ($tmp === 0) Result = "Null";
      return Result;
    };
    this.ReadInt8 = function () {
      var Result = 0;
      this.FStream.ReadBufferData$4({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }});
      return Result;
    };
    this.ReadInt16 = function () {
      var Result = 0;
      this.FStream.ReadBufferData$8({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }});
      return Result;
    };
    this.ReadInt32 = function () {
      var Result = 0;
      this.FStream.ReadBufferData$12({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }});
      return Result;
    };
    this.ReadNativeInt = function () {
      var Result = 0;
      this.FStream.ReadBufferData$16({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }});
      return Result;
    };
    this.ReadSet = function (EnumType) {
      var Result = 0;
      var Name = "";
      var Value = 0;
      try {
        Result = 0;
        while (true) {
          Name = this.ReadStr();
          if (Name.length === 0) break;
          Value = EnumType.enumtype[Name];
          if (Value === -1) throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyValue")]);
          Result = Result | (1 << Value);
        };
      } catch ($e) {
        this.SkipSetBody();
        throw $e;
      };
      return Result;
    };
    this.ReadSignature = function () {
      var Signature = 0;
      this.FStream.ReadBufferData$12({get: function () {
          return Signature;
        }, set: function (v) {
          Signature = v;
        }});
      if (Signature !== 809914452) throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidImage")]);
    };
    this.ReadStr = function () {
      var Result = "";
      var l = 0;
      var i = 0;
      var c = "";
      this.FStream.ReadBufferData$6({get: function () {
          return l;
        }, set: function (v) {
          l = v;
        }});
      Result = rtl.strSetLength(Result,l);
      for (var $l = 1, $end = l; $l <= $end; $l++) {
        i = $l;
        this.FStream.ReadBufferData$2({get: function () {
            return c;
          }, set: function (v) {
            c = v;
          }});
        Result = rtl.setCharAt(Result,i - 1,c);
      };
      return Result;
    };
    this.ReadString = function (StringType) {
      var Result = "";
      var i = 0;
      var C = "";
      Result = "";
      if (StringType !== 6) throw $mod.EFilerError.$create("Create$1",["Invalid string type passed to ReadString"]);
      i = this.ReadDWord();
      Result = rtl.strSetLength(Result,i);
      for (var $l = 1, $end = Result.length; $l <= $end; $l++) {
        i = $l;
        this.FStream.ReadBufferData$2({get: function () {
            return C;
          }, set: function (v) {
            C = v;
          }});
        Result = rtl.setCharAt(Result,i - 1,C);
      };
      return Result;
    };
    this.SkipComponent = function (SkipComponentInfos) {
      var Flags = {};
      var Dummy = 0;
      var CompClassName = "";
      var CompName = "";
      if (SkipComponentInfos) this.BeginComponent({get: function () {
          return Flags;
        }, set: function (v) {
          Flags = v;
        }},{get: function () {
          return Dummy;
        }, set: function (v) {
          Dummy = v;
        }},{get: function () {
          return CompClassName;
        }, set: function (v) {
          CompClassName = v;
        }},{get: function () {
          return CompName;
        }, set: function (v) {
          CompName = v;
        }});
      while (this.NextValue() !== 0) this.SkipProperty();
      this.ReadValue();
      while (this.NextValue() !== 0) this.SkipComponent(true);
      this.ReadValue();
    };
    this.SkipValue = function () {
      var $Self = this;
      function SkipBytes(Count) {
        var Dummy = [];
        var SkipNow = 0;
        while (Count > 0) {
          if (Count > 1024) {
            SkipNow = 1024}
           else SkipNow = Count;
          Dummy = rtl.arraySetLength(Dummy,0,SkipNow);
          $Self.Read({get: function () {
              return Dummy;
            }, set: function (v) {
              Dummy = v;
            }},SkipNow);
          Count -= SkipNow;
        };
      };
      var Count = 0;
      var $tmp = this.ReadValue();
      if (($tmp === 0) || ($tmp === 8) || ($tmp === 9) || ($tmp === 12)) {}
      else if ($tmp === 1) {
        while (this.NextValue() !== 0) this.SkipValue();
        this.ReadValue();
      } else if ($tmp === 2) {
        SkipBytes(1)}
       else if ($tmp === 3) {
        SkipBytes(2)}
       else if ($tmp === 4) {
        SkipBytes(4)}
       else if (($tmp === $mod.TValueType.vaNativeInt) || ($tmp === 5)) {
        SkipBytes(8)}
       else if ($tmp === 7) {
        this.ReadStr()}
       else if ($tmp === 6) {
        this.ReadString(6)}
       else if ($tmp === 10) {
        Count = this.ReadDWord() & 0xFFFFFFFF;
        SkipBytes(Count);
      } else if ($tmp === 11) {
        this.SkipSetBody()}
       else if ($tmp === 13) {
        while (this.NextValue() !== 0) {
          if (this.NextValue() in rtl.createSet(2,3,4)) this.SkipValue();
          SkipBytes(1);
          while (this.NextValue() !== 0) this.SkipProperty();
          this.ReadValue();
        };
        this.ReadValue();
      };
    };
  });
  rtl.createClass(this,"TReader",this.TFiler,function () {
    this.$init = function () {
      $mod.TFiler.$init.call(this);
      this.FDriver = null;
      this.FOwner = null;
      this.FParent = null;
      this.FFixups = null;
      this.FLoaded = null;
      this.FOnFindMethod = null;
      this.FOnSetMethodProperty = null;
      this.FOnSetName = null;
      this.FOnReferenceName = null;
      this.FOnAncestorNotFound = null;
      this.FOnError = null;
      this.FOnPropertyNotFound = null;
      this.FOnFindComponentClass = null;
      this.FOnCreateComponent = null;
      this.FPropName = "";
      this.FCanHandleExcepts = false;
      this.FOnReadStringProperty = null;
    };
    this.$final = function () {
      this.FDriver = undefined;
      this.FOwner = undefined;
      this.FParent = undefined;
      this.FFixups = undefined;
      this.FLoaded = undefined;
      this.FOnFindMethod = undefined;
      this.FOnSetMethodProperty = undefined;
      this.FOnSetName = undefined;
      this.FOnReferenceName = undefined;
      this.FOnAncestorNotFound = undefined;
      this.FOnError = undefined;
      this.FOnPropertyNotFound = undefined;
      this.FOnFindComponentClass = undefined;
      this.FOnCreateComponent = undefined;
      this.FOnReadStringProperty = undefined;
      $mod.TFiler.$final.call(this);
    };
    this.DoFixupReferences = function () {
      var R = null;
      var RN = null;
      var G = null;
      var Ref = "";
      var C = null;
      var P = 0;
      var L = null;
      var $ir = rtl.createIntfRefs();
      try {
        if (this.FFixups != null) {
          L = this.FFixups;
          R = L.FRoot;
          while (R !== null) {
            RN = R.Next;
            Ref = R.FRelative;
            if (this.FOnReferenceName != null) this.FOnReferenceName(this,{get: function () {
                return Ref;
              }, set: function (v) {
                Ref = v;
              }});
            C = $mod.FindNestedComponent(R.FRoot,Ref,true);
            if (C != null) {
              if (R.FPropInfo.typeinfo.kind === 18) {
                pas.TypInfo.SetInterfaceProp$1(R.Finstance,R.FPropInfo,$ir.ref(1,rtl.queryIntfT(C,pas.System.IUnknown)))}
               else pas.TypInfo.SetObjectProp$1(R.Finstance,R.FPropInfo,C)}
             else {
              P = pas.System.Pos(".",R.FRelative);
              if (P !== 0) {
                G = $impl.AddtoResolveList(R.Finstance);
                G.AddReference(R.FRoot,R.FPropInfo,pas.System.Copy(R.FRelative,1,P - 1),pas.System.Copy(R.FRelative,P + 1,R.FRelative.length - P));
              };
            };
            L.RemoveItem(R,true);
            R = RN;
          };
          pas.SysUtils.FreeAndNil({p: this, get: function () {
              return this.p.FFixups;
            }, set: function (v) {
              this.p.FFixups = v;
            }});
        };
      } finally {
        $ir.free();
      };
    };
    this.FindComponentClass = function (AClassName) {
      var $Self = this;
      var Result = null;
      var PersistentClass = null;
      function FindClassInFieldTable(Instance) {
        var Result = null;
        var aClass = null;
        var i = 0;
        var ClassTI = null;
        var MemberClassTI = null;
        var MemberTI = null;
        aClass = Instance.$class.ClassType();
        while (aClass !== null) {
          ClassTI = aClass.$rtti;
          for (var $l = 0, $end = ClassTI.fields.length - 1; $l <= $end; $l++) {
            i = $l;
            MemberTI = ClassTI.getField(i).typeinfo;
            if (MemberTI.kind === 13) {
              MemberClassTI = MemberTI;
              if (pas.SysUtils.SameText(MemberClassTI.name,AClassName) && rtl.is(MemberClassTI.class,$mod.TComponent)) return MemberClassTI.class;
            };
          };
          aClass = aClass.$ancestor;
        };
        return Result;
      };
      Result = null;
      Result = FindClassInFieldTable(this.FRoot);
      if ((Result === null) && (this.FLookupRoot != null) && (this.FLookupRoot !== this.FRoot)) Result = FindClassInFieldTable(this.FLookupRoot);
      if (Result === null) {
        PersistentClass = $mod.GetClass(AClassName);
        if (PersistentClass.InheritsFrom($mod.TComponent)) Result = PersistentClass;
      };
      if ((Result === null) && (this.FOnFindComponentClass != null)) this.FOnFindComponentClass($Self,AClassName,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }});
      if ((Result === null) || !Result.InheritsFrom($mod.TComponent)) throw $mod.EClassNotFound.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SClassNotFound"),pas.System.VarRecs(18,AClassName)]);
      return Result;
    };
    this.Error = function (Message) {
      var Result = false;
      Result = false;
      if (this.FOnError != null) this.FOnError(this,Message,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }});
      return Result;
    };
    this.FindMethod = function (ARoot, AMethodName) {
      var Result = null;
      var ErrorResult = false;
      Result = null;
      if ((ARoot === null) || (AMethodName === "")) return Result;
      Result = ARoot.$class.MethodAddress(AMethodName);
      ErrorResult = Result === null;
      if (this.FOnFindMethod != null) this.FOnFindMethod(this,AMethodName,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},{get: function () {
          return ErrorResult;
        }, set: function (v) {
          ErrorResult = v;
        }});
      if (ErrorResult) throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyValue")]);
      return Result;
    };
    this.ReadProperty = function (AInstance) {
      var $Self = this;
      var Path = "";
      var Instance = null;
      var PropInfo = null;
      var Obj = null;
      var Name = "";
      var Skip = false;
      var Handled = false;
      var OldPropName = "";
      var DotPos = "";
      var NextPos = 0;
      function HandleMissingProperty(IsPath) {
        var Result = false;
        Result = true;
        if ($Self.FOnPropertyNotFound != null) {
          OldPropName = $Self.FPropName;
          Handled = false;
          Skip = false;
          $Self.FOnPropertyNotFound($Self,Instance,{p: $Self, get: function () {
              return this.p.FPropName;
            }, set: function (v) {
              this.p.FPropName = v;
            }},IsPath,{get: function () {
              return Handled;
            }, set: function (v) {
              Handled = v;
            }},{get: function () {
              return Skip;
            }, set: function (v) {
              Skip = v;
            }});
          if (Handled && !Skip && (OldPropName !== $Self.FPropName)) PropInfo = pas.TypInfo.GetPropInfo$4(Instance.$class.ClassType(),$Self.FPropName);
          if (Skip) {
            $Self.FDriver.SkipValue();
            Result = false;
            return Result;
          };
        };
        return Result;
      };
      try {
        Path = this.FDriver.BeginProperty();
        try {
          Instance = AInstance;
          this.FCanHandleExcepts = true;
          DotPos = Path;
          while (true) {
            NextPos = pas.System.Pos(".",DotPos);
            if (NextPos > 0) {
              this.FPropName = pas.System.Copy(DotPos,1,NextPos - 1)}
             else {
              this.FPropName = DotPos;
              break;
            };
            pas.System.Delete({get: function () {
                return DotPos;
              }, set: function (v) {
                DotPos = v;
              }},1,NextPos);
            PropInfo = pas.TypInfo.GetPropInfo$4(Instance.$class.ClassType(),this.FPropName);
            if (!(PropInfo != null)) {
              if (!HandleMissingProperty(true)) return;
              if (!(PropInfo != null)) this.PropertyError();
            };
            if (PropInfo.typeinfo.kind === 13) {
              Obj = pas.TypInfo.GetObjectProp$2(Instance,PropInfo)}
             else Obj = null;
            if (!$mod.TPersistent.isPrototypeOf(Obj)) {
              this.FDriver.SkipValue();
              throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyPath")]);
            };
            Instance = Obj;
          };
          PropInfo = pas.TypInfo.GetPropInfo$4(Instance.$class.ClassType(),this.FPropName);
          if (PropInfo != null) {
            this.ReadPropValue(Instance,PropInfo)}
           else {
            this.FCanHandleExcepts = false;
            Instance.DefineProperties($Self);
            this.FCanHandleExcepts = true;
            if (this.FPropName.length > 0) {
              if (!HandleMissingProperty(false)) return;
              if (!(PropInfo != null)) this.PropertyError();
            };
          };
        } catch ($e) {
          if (pas.SysUtils.Exception.isPrototypeOf($e)) {
            var e = $e;
            Name = rtl.strSetLength(Name,0);
            if (AInstance.$class.InheritsFrom($mod.TComponent)) Name = AInstance.FName;
            if (Name.length === 0) Name = AInstance.$classname;
            throw $mod.EReadError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SPropertyException"),pas.System.VarRecs(18,Name,9,".",18,Path,18,e.fMessage)]);
          } else throw $e
        };
      } catch ($e) {
        if (pas.SysUtils.Exception.isPrototypeOf($e)) {
          var e = $e;
          if (!this.FCanHandleExcepts || !this.Error(e.fMessage)) throw $e;
        } else throw $e
      };
    };
    var NullMethod = pas.System.TMethod.$clone({Code: null, Data: null});
    this.ReadPropValue = function (Instance, PropInfo) {
      var PropType = null;
      var Value = 0;
      var Ident = "";
      var Method = pas.System.TMethod.$new();
      var Handled = false;
      var TmpStr = "";
      if (PropInfo.setter === "") throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SReadOnlyProperty")]);
      PropType = PropInfo.typeinfo;
      var $tmp = PropType.kind;
      if ($tmp === 1) {
        var $tmp1 = this.FDriver.NextValue();
        if ($tmp1 === 7) {
          Ident = this.ReadIdent();
          if ($impl.GlobalIdentToInt(Ident,{get: function () {
              return Value;
            }, set: function (v) {
              Value = v;
            }})) {
            pas.TypInfo.SetOrdProp$1(Instance,PropInfo,Value)}
           else throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyValue")]);
        } else if ($tmp1 === 16) {
          pas.TypInfo.SetOrdProp$1(Instance,PropInfo,this.ReadNativeInt())}
         else if ($tmp1 === 14) {
          pas.TypInfo.SetFloatProp$1(Instance,PropInfo,this.ReadCurrency() / 10000)}
         else {
          pas.TypInfo.SetOrdProp$1(Instance,PropInfo,this.ReadInteger());
        };
      } else if ($tmp === 7) {
        pas.TypInfo.SetOrdProp$1(Instance,PropInfo,this.ReadBoolean() + 0)}
       else if ($tmp === 2) {
        pas.TypInfo.SetOrdProp$1(Instance,PropInfo,this.ReadChar().charCodeAt())}
       else if ($tmp === 4) {
        Value = pas.TypInfo.GetEnumValue(PropType,this.ReadIdent());
        if (Value === -1) throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyValue")]);
        pas.TypInfo.SetOrdProp$1(Instance,PropInfo,Value);
      } else if ($tmp === pas.System.TTypeKind.tkDouble) {
        pas.TypInfo.SetFloatProp$1(Instance,PropInfo,this.ReadFloat())}
       else if ($tmp === 5) {
        this.CheckValue(11);
        if (PropType.comptype.kind === 4) pas.TypInfo.SetOrdProp$1(Instance,PropInfo,this.FDriver.ReadSet(PropType.comptype));
      } else if (($tmp === 9) || ($tmp === 17)) {
        if (this.FDriver.NextValue() === 12) {
          this.FDriver.ReadValue();
          pas.TypInfo.SetMethodProp(Instance,PropInfo,NullMethod);
        } else {
          Handled = false;
          Ident = this.ReadIdent();
          if (this.FOnSetMethodProperty != null) this.FOnSetMethodProperty(this,Instance,PropInfo,Ident,{get: function () {
              return Handled;
            }, set: function (v) {
              Handled = v;
            }});
          if (!Handled) {
            Method.Code = this.FindMethod(this.FRoot,Ident);
            Method.Data = this.FRoot;
            if (Method.Code != null) pas.TypInfo.SetMethodProp(Instance,PropInfo,Method);
          };
        }}
       else if ($tmp === 3) {
        TmpStr = this.ReadString();
        if (this.FOnReadStringProperty != null) this.FOnReadStringProperty(this,Instance,PropInfo,{get: function () {
            return TmpStr;
          }, set: function (v) {
            TmpStr = v;
          }});
        pas.TypInfo.SetStrProp$1(Instance,PropInfo,TmpStr);
      } else if ($tmp === 16) {
        pas.TypInfo.SetJSValueProp$3(Instance,PropInfo,this.ReadVariant());
      } else if (($tmp === 13) || ($tmp === 18)) {
        var $tmp2 = this.FDriver.NextValue();
        if ($tmp2 === 12) {
          this.FDriver.ReadValue();
          pas.TypInfo.SetOrdProp$1(Instance,PropInfo,0);
        } else if ($tmp2 === 13) {
          this.FDriver.ReadValue();
          this.ReadCollection(pas.TypInfo.GetObjectProp$2(Instance,PropInfo));
        } else {
          if (!(this.FFixups != null)) this.FFixups = pas.simplelinkedlist.TLinkedList.$create("Create$1",[$impl.TLocalUnResolvedReference]);
          var $with = this.FFixups.Add();
          $with.Finstance = Instance;
          $with.FRoot = this.FRoot;
          $with.FPropInfo = PropInfo;
          $with.FRelative = this.ReadIdent();
        };
      } else {
        throw $mod.EReadError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SUnknownPropertyType"),pas.System.VarRecs(18,pas.System.TTypeKind[PropType.kind])]);
      };
    };
    this.PropertyError = function () {
      this.FDriver.SkipValue();
      throw $mod.EReadError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SUnknownProperty"),pas.System.VarRecs(18,this.FPropName)]);
    };
    this.ReadData = function (Instance) {
      var SavedOwner = null;
      var SavedParent = null;
      while (!this.EndOfList()) this.ReadProperty(Instance);
      this.ReadListEnd();
      SavedOwner = this.FOwner;
      SavedParent = this.FParent;
      try {
        this.FOwner = Instance.GetChildOwner();
        if (!(this.FOwner != null)) this.FOwner = this.FRoot;
        this.FParent = Instance.GetChildParent();
        while (!this.EndOfList()) this.ReadComponent(null);
        this.ReadListEnd();
      } finally {
        this.FOwner = SavedOwner;
        this.FParent = SavedParent;
      };
      if (Instance === this.FRoot) this.DoFixupReferences();
    };
    this.CreateDriver = function (Stream) {
      var Result = null;
      Result = $mod.TBinaryObjectReader.$create("Create$1",[Stream]);
      return Result;
    };
    this.Create$1 = function (Stream) {
      pas.System.TObject.Create.call(this);
      if (Stream === null) throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SEmptyStreamIllegalReader")]);
      this.FDriver = this.CreateDriver(Stream);
      return this;
    };
    this.Destroy = function () {
      rtl.free(this,"FDriver");
      pas.System.TObject.Destroy.call(this);
    };
    this.CheckValue = function (Value) {
      if (this.FDriver.NextValue() !== Value) {
        throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyValue")])}
       else this.FDriver.ReadValue();
    };
    this.DefineProperty = function (Name, AReadData, WriteData, HasData) {
      if ((AReadData != null) && pas.SysUtils.SameText(Name,this.FPropName)) {
        AReadData(this);
        this.FPropName = rtl.strSetLength(this.FPropName,0);
      } else if ((WriteData != null) && HasData) ;
    };
    this.EndOfList = function () {
      var Result = false;
      Result = this.FDriver.NextValue() === 0;
      return Result;
    };
    this.NextValue = function () {
      var Result = 0;
      Result = this.FDriver.NextValue();
      return Result;
    };
    this.ReadBoolean = function () {
      var Result = false;
      var ValueType = 0;
      ValueType = this.FDriver.ReadValue();
      if (ValueType === 9) {
        Result = true}
       else if (ValueType === 8) {
        Result = false}
       else throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyValue")]);
      return Result;
    };
    this.ReadChar = function () {
      var Result = "";
      var s = "";
      s = this.ReadString();
      if (s.length === 1) {
        Result = s.charAt(0)}
       else throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyValue")]);
      return Result;
    };
    this.ReadCollection = function (Collection) {
      var Item = null;
      Collection.BeginUpdate();
      if (!this.EndOfList()) Collection.Clear();
      while (!this.EndOfList()) {
        this.ReadListBegin();
        Item = Collection.Add();
        while (this.NextValue() !== 0) this.ReadProperty(Item);
        this.ReadListEnd();
      };
      Collection.EndUpdate();
      this.ReadListEnd();
    };
    this.ReadComponent = function (Component) {
      var $Self = this;
      var Result = null;
      var Flags = {};
      function Recover(E, aComponent) {
        var Result = false;
        Result = false;
        if (!((0 in Flags) || (Component != null))) aComponent.set(rtl.freeLoc(aComponent.get()));
        aComponent.set(null);
        $Self.FDriver.SkipComponent(false);
        Result = $Self.Error(E.fMessage);
        return Result;
      };
      var CompClassName = "";
      var Name = "";
      var n = 0;
      var ChildPos = 0;
      var SavedParent = null;
      var SavedLookupRoot = null;
      var ComponentClass = null;
      var C = null;
      var NewComponent = null;
      var SubComponents = null;
      this.FDriver.BeginComponent({get: function () {
          return Flags;
        }, set: function (v) {
          Flags = v;
        }},{get: function () {
          return ChildPos;
        }, set: function (v) {
          ChildPos = v;
        }},{get: function () {
          return CompClassName;
        }, set: function (v) {
          CompClassName = v;
        }},{get: function () {
          return Name;
        }, set: function (v) {
          Name = v;
        }});
      SavedParent = this.FParent;
      SavedLookupRoot = this.FLookupRoot;
      SubComponents = null;
      try {
        Result = Component;
        if (!(Result != null)) try {
          if (0 in Flags) {
            if (this.FLookupRoot != null) {
              Result = this.FLookupRoot.FindComponent(Name)}
             else Result = null;
            if (!(Result != null)) {
              if (this.FOnAncestorNotFound != null) this.FOnAncestorNotFound($Self,Name,this.FindComponentClass(CompClassName),{get: function () {
                  return Result;
                }, set: function (v) {
                  Result = v;
                }});
              if (!(Result != null)) throw $mod.EReadError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SAncestorNotFound"),pas.System.VarRecs(18,Name)]);
            };
            this.FParent = Result.GetParentComponent();
            if (!(this.FParent != null)) this.FParent = this.FRoot;
          } else {
            Result = null;
            ComponentClass = this.FindComponentClass(CompClassName);
            if (this.FOnCreateComponent != null) this.FOnCreateComponent($Self,ComponentClass,{get: function () {
                return Result;
              }, set: function (v) {
                Result = v;
              }});
            if (!(Result != null)) {
              NewComponent = Object.create(ComponentClass);
              NewComponent.$init();
              if (2 in Flags) NewComponent.FComponentState = rtl.unionSet(NewComponent.FComponentState,rtl.createSet(0,9));
              NewComponent.Create$1(this.FOwner);
              NewComponent.AfterConstruction();
              Result = NewComponent;
            };
            Result.FComponentState = rtl.includeSet(Result.FComponentState,0);
          };
        } catch ($e) {
          if (pas.SysUtils.Exception.isPrototypeOf($e)) {
            var E = $e;
            if (!Recover(E,{get: function () {
                return Result;
              }, set: function (v) {
                Result = v;
              }})) throw $e;
          } else throw $e
        };
        if (Result != null) try {
          Result.FComponentState = rtl.includeSet(Result.FComponentState,0);
          SubComponents = $mod.TList.$create("Create$1");
          for (var $l = 0, $end = Result.GetComponentCount() - 1; $l <= $end; $l++) {
            n = $l;
            C = Result.GetComponent(n);
            if (2 in C.FComponentStyle) {
              SubComponents.Add(C);
              C.FComponentState = rtl.includeSet(C.FComponentState,0);
            };
          };
          if (!(0 in Flags)) try {
            Result.SetParentComponent(this.FParent);
            if (this.FOnSetName != null) this.FOnSetName($Self,Result,{get: function () {
                return Name;
              }, set: function (v) {
                Name = v;
              }});
            Result.SetName(Name);
            if ($mod.FindGlobalComponent(Name) === Result) Result.FComponentState = rtl.includeSet(Result.FComponentState,9);
          } catch ($e) {
            if (pas.SysUtils.Exception.isPrototypeOf($e)) {
              var E = $e;
              if (!Recover(E,{get: function () {
                  return Result;
                }, set: function (v) {
                  Result = v;
                }})) throw $e;
            } else throw $e
          };
          if (!(Result != null)) return Result;
          if (9 in Result.FComponentState) this.FLookupRoot = Result;
          Result.FComponentState = rtl.includeSet(Result.FComponentState,1);
          for (var $l1 = 0, $end1 = SubComponents.GetCount() - 1; $l1 <= $end1; $l1++) {
            n = $l1;
            rtl.getObject(SubComponents.Get(n)).FComponentState = rtl.includeSet(rtl.getObject(SubComponents.Get(n)).FComponentState,1);
          };
          Result.ReadState($Self);
          Result.FComponentState = rtl.excludeSet(Result.FComponentState,1);
          for (var $l2 = 0, $end2 = SubComponents.GetCount() - 1; $l2 <= $end2; $l2++) {
            n = $l2;
            rtl.getObject(SubComponents.Get(n)).FComponentState = rtl.excludeSet(rtl.getObject(SubComponents.Get(n)).FComponentState,1);
          };
          if (1 in Flags) this.FParent.SetChildOrder(Result,ChildPos);
          if (!((0 in Flags) || (9 in Result.FComponentState)) || (this.FLoaded.IndexOf(Result) < 0)) {
            for (var $l3 = 0, $end3 = SubComponents.GetCount() - 1; $l3 <= $end3; $l3++) {
              n = $l3;
              this.FLoaded.Add(SubComponents.Get(n));
            };
            this.FLoaded.Add(Result);
          };
        } catch ($e) {
          if ((0 in Flags) || (Component != null)) Result = rtl.freeLoc(Result);
          throw $e;
        };
      } finally {
        this.FParent = SavedParent;
        this.FLookupRoot = SavedLookupRoot;
        SubComponents = rtl.freeLoc(SubComponents);
      };
      return Result;
    };
    this.ReadFloat = function () {
      var Result = 0.0;
      if (this.FDriver.NextValue() === $mod.TValueType.vaDouble) {
        this.ReadValue();
        Result = this.FDriver.ReadFloat();
      } else Result = this.ReadNativeInt();
      return Result;
    };
    this.ReadCurrency = function () {
      var Result = 0;
      if (this.FDriver.NextValue() === 14) {
        this.FDriver.ReadValue();
        Result = this.FDriver.ReadCurrency();
      } else Result = this.ReadInteger() * 10000;
      return Result;
    };
    this.ReadIdent = function () {
      var Result = "";
      var ValueType = 0;
      ValueType = this.FDriver.ReadValue();
      if (ValueType in rtl.createSet(7,12,8,9,0)) {
        Result = this.FDriver.ReadIdent(ValueType)}
       else throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyValue")]);
      return Result;
    };
    this.ReadInteger = function () {
      var Result = 0;
      var $tmp = this.FDriver.ReadValue();
      if ($tmp === 2) {
        Result = this.FDriver.ReadInt8()}
       else if ($tmp === 3) {
        Result = this.FDriver.ReadInt16()}
       else if ($tmp === 4) {
        Result = this.FDriver.ReadInt32()}
       else {
        throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyValue")]);
      };
      return Result;
    };
    this.ReadNativeInt = function () {
      var Result = 0;
      if (this.FDriver.NextValue() === $mod.TValueType.vaNativeInt) {
        this.FDriver.ReadValue();
        Result = this.FDriver.ReadNativeInt();
      } else Result = this.ReadInteger();
      return Result;
    };
    this.ReadListBegin = function () {
      this.CheckValue(1);
    };
    this.ReadListEnd = function () {
      this.CheckValue(0);
    };
    this.ReadRootComponent = function (ARoot) {
      var Result = null;
      var Dummy = 0;
      var i = 0;
      var Flags = {};
      var CompClassName = "";
      var CompName = "";
      var ResultName = "";
      this.FDriver.BeginRootComponent();
      Result = null;
      try {
        this.FDriver.BeginComponent({get: function () {
            return Flags;
          }, set: function (v) {
            Flags = v;
          }},{get: function () {
            return Dummy;
          }, set: function (v) {
            Dummy = v;
          }},{get: function () {
            return CompClassName;
          }, set: function (v) {
            CompClassName = v;
          }},{get: function () {
            return CompName;
          }, set: function (v) {
            CompName = v;
          }});
        if (!(ARoot != null)) {
          Result = $mod.FindClass(CompClassName).$create("Create$1",[null]);
          Result.SetName(CompName);
        } else {
          Result = ARoot;
          if (!(4 in Result.FComponentState)) {
            Result.FComponentState = rtl.unionSet(Result.FComponentState,rtl.createSet(0,1));
            i = 0;
            ResultName = CompName;
            while ($mod.FindGlobalComponent(ResultName) != null) {
              i += 1;
              ResultName = CompName + "_" + pas.SysUtils.IntToStr(i);
            };
            Result.SetName(ResultName);
          };
        };
        this.FRoot = Result;
        this.FLookupRoot = Result;
        if ($impl.GlobalLoaded != null) {
          this.FLoaded = $impl.GlobalLoaded}
         else this.FLoaded = $mod.TFPList.$create("Create");
        try {
          if (this.FLoaded.IndexOf(this.FRoot) < 0) this.FLoaded.Add(this.FRoot);
          this.FOwner = this.FRoot;
          this.FRoot.FComponentState = rtl.unionSet(this.FRoot.FComponentState,rtl.createSet(0,1));
          this.FRoot.ReadState(this);
          this.FRoot.FComponentState = rtl.excludeSet(this.FRoot.FComponentState,1);
          if (!($impl.GlobalLoaded != null)) for (var $l = 0, $end = this.FLoaded.FCount - 1; $l <= $end; $l++) {
            i = $l;
            rtl.getObject(this.FLoaded.Get(i)).Loaded();
          };
        } finally {
          if (!($impl.GlobalLoaded != null)) rtl.free(this,"FLoaded");
          this.FLoaded = null;
        };
        $impl.GlobalFixupReferences();
      } catch ($e) {
        $mod.RemoveFixupReferences(ARoot,"");
        if (!(ARoot != null)) Result = rtl.freeLoc(Result);
        throw $e;
      };
      return Result;
    };
    this.ReadVariant = function () {
      var Result = undefined;
      var nv = 0;
      nv = this.NextValue();
      var $tmp = nv;
      if ($tmp === 12) {
        Result = undefined;
        this.ReadValue();
      } else if ($tmp === 0) {
        Result = null;
        this.ReadValue();
      } else if (($tmp === 2) || ($tmp === 3) || ($tmp === 4)) {
        Result = this.ReadInteger();
      } else if ($tmp === $mod.TValueType.vaNativeInt) {
        Result = this.ReadNativeInt();
      } else if (($tmp === 8) || ($tmp === 9)) {
        Result = nv !== 8;
        this.ReadValue();
      } else if ($tmp === 14) {
        Result = this.ReadCurrency() / 10000;
      } else if ($tmp === 5) {
        Result = this.ReadFloat();
      } else if ($tmp === 6) {
        Result = this.ReadString();
      } else {
        throw $mod.EReadError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SUnsupportedPropertyVariantType"),pas.System.VarRecs(0,nv)]);
      };
      return Result;
    };
    this.ReadString = function () {
      var Result = "";
      var StringType = 0;
      StringType = this.FDriver.ReadValue();
      if (StringType === 6) {
        Result = this.FDriver.ReadString(StringType)}
       else throw $mod.EReadError.$create("Create$1",[rtl.getResStr(pas.RTLConsts,"SInvalidPropertyValue")]);
      return Result;
    };
    this.ReadValue = function () {
      var Result = 0;
      Result = this.FDriver.ReadValue();
      return Result;
    };
  });
  rtl.createClass(this,"TAbstractObjectWriter",pas.System.TObject,function () {
  });
  rtl.createClass(this,"TWriter",this.TFiler,function () {
    this.$init = function () {
      $mod.TFiler.$init.call(this);
      this.FDriver = null;
      this.FDestroyDriver = false;
      this.FPropPath = "";
    };
    this.$final = function () {
      this.FDriver = undefined;
      $mod.TFiler.$final.call(this);
    };
    this.Destroy = function () {
      if (this.FDestroyDriver) rtl.free(this,"FDriver");
      pas.System.TObject.Destroy.call(this);
    };
    this.DefineProperty = function (Name, ReadData, AWriteData, HasData) {
      if (HasData && (AWriteData != null)) {
        this.FDriver.BeginProperty(this.FPropPath + Name);
        AWriteData(this);
        this.FDriver.EndProperty();
      } else if (ReadData != null) ;
    };
    this.WriteInteger = function (Value) {
      this.FDriver.WriteInteger(Value);
    };
    this.WriteListBegin = function () {
      this.FDriver.BeginList();
    };
    this.WriteListEnd = function () {
      this.FDriver.EndList();
    };
    this.WriteString = function (Value) {
      this.FDriver.WriteString(Value);
    };
  });
  this.TParserToken = {"0": "toUnknown", toUnknown: 0, "1": "toEOF", toEOF: 1, "2": "toSymbol", toSymbol: 2, "3": "ToString", ToString: 3, "4": "toInteger", toInteger: 4, "5": "toFloat", toFloat: 5, "6": "toMinus", toMinus: 6, "7": "toSetStart", toSetStart: 7, "8": "toListStart", toListStart: 8, "9": "toCollectionStart", toCollectionStart: 9, "10": "toBinaryStart", toBinaryStart: 10, "11": "toSetEnd", toSetEnd: 11, "12": "toListEnd", toListEnd: 12, "13": "toCollectionEnd", toCollectionEnd: 13, "14": "toBinaryEnd", toBinaryEnd: 14, "15": "toComma", toComma: 15, "16": "toDot", toDot: 16, "17": "toEqual", toEqual: 17, "18": "toColon", toColon: 18, "19": "toPlus", toPlus: 19};
  rtl.createClass(this,"TParser",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.fStream = null;
      this.fBuf = [];
      this.FBufLen = 0;
      this.fPos = 0;
      this.fDeltaPos = 0;
      this.fFloatType = "";
      this.fSourceLine = 0;
      this.fToken = 0;
      this.fEofReached = false;
      this.fLastTokenStr = "";
    };
    this.$final = function () {
      this.fStream = undefined;
      this.fBuf = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.GetTokenName = function (aTok) {
      var Result = "";
      Result = $impl.TokNames[aTok];
      return Result;
    };
    this.LoadBuffer = function () {
      var CharsRead = 0;
      var i = 0;
      CharsRead = 0;
      for (i = 0; i <= 4095; i++) {
        if (this.fStream.ReadData$3({a: i, p: this.fBuf, get: function () {
            return this.p[this.a];
          }, set: function (v) {
            this.p[this.a] = v;
          }}) !== 2) break;
        CharsRead += 1;
      };
      this.fDeltaPos += CharsRead;
      this.fPos = 0;
      this.FBufLen = CharsRead;
      this.fEofReached = CharsRead === 0;
    };
    this.CheckLoadBuffer = function () {
      if (this.fPos >= this.FBufLen) this.LoadBuffer();
    };
    this.ProcessChar = function () {
      this.fLastTokenStr = this.fLastTokenStr + this.fBuf[this.fPos];
      this.GotoToNextChar();
    };
    this.IsNumber = function () {
      var Result = false;
      Result = this.fBuf[this.fPos].charCodeAt() in rtl.createSet(null,48,57);
      return Result;
    };
    this.IsHexNum = function () {
      var Result = false;
      Result = this.fBuf[this.fPos].charCodeAt() in rtl.createSet(null,48,57,null,65,70,null,97,102);
      return Result;
    };
    this.IsAlpha = function () {
      var Result = false;
      Result = this.fBuf[this.fPos].charCodeAt() in rtl.createSet(95,null,65,90,null,97,122);
      return Result;
    };
    this.IsAlphaNum = function () {
      var Result = false;
      Result = this.IsAlpha() || this.IsNumber();
      return Result;
    };
    this.GetHexValue = function (c) {
      var Result = 0;
      var $tmp = c;
      if (($tmp >= "0") && ($tmp <= "9")) {
        Result = c.charCodeAt() - 0x30}
       else if (($tmp >= "A") && ($tmp <= "F")) {
        Result = c.charCodeAt() - 0x37}
       else if (($tmp >= "a") && ($tmp <= "f")) Result = c.charCodeAt() - 0x57;
      return Result;
    };
    this.GetAlphaNum = function () {
      var Result = "";
      if (!this.IsAlpha()) this.ErrorFmt(rtl.getResStr(pas.RTLConsts,"SParserExpected"),pas.System.VarRecs(18,this.GetTokenName(2)));
      Result = "";
      while (this.IsAlphaNum()) {
        Result = Result + this.fBuf[this.fPos];
        this.GotoToNextChar();
      };
      return Result;
    };
    this.HandleNewLine = function () {
      if (this.fBuf[this.fPos] === "\r") this.GotoToNextChar();
      if (this.fBuf[this.fPos] === "\n") this.GotoToNextChar();
      this.fSourceLine += 1;
      this.fDeltaPos = -(this.fPos - 1);
    };
    this.SkipBOM = function () {
    };
    this.SkipSpaces = function () {
      while (!this.fEofReached && (this.fBuf[this.fPos].charCodeAt() in rtl.createSet(32,9))) this.GotoToNextChar();
    };
    this.SkipWhitespace = function () {
      while (!this.fEofReached) {
        var $tmp = this.fBuf[this.fPos];
        if (($tmp === " ") || ($tmp === "\t")) {
          this.SkipSpaces()}
         else if (($tmp === "\n") || ($tmp === "\r")) {
          this.HandleNewLine()}
         else {
          break;
        };
      };
    };
    this.HandleEof = function () {
      this.fToken = 1;
      this.fLastTokenStr = "";
    };
    this.HandleAlphaNum = function () {
      this.fLastTokenStr = this.GetAlphaNum();
      this.fToken = 2;
    };
    var floatPunct = {"0": "fpDot", fpDot: 0, "1": "fpE", fpE: 1};
    this.HandleNumber = function () {
      var allowed = {};
      this.fLastTokenStr = "";
      while (this.IsNumber()) this.ProcessChar();
      this.fToken = 4;
      if (this.fBuf[this.fPos].charCodeAt() in rtl.createSet(46,101,69)) {
        this.fToken = 5;
        allowed = rtl.createSet(0,1);
        while (this.fBuf[this.fPos].charCodeAt() in rtl.createSet(46,101,69,null,48,57)) {
          var $tmp = this.fBuf[this.fPos];
          if ($tmp === ".") {
            if (0 in allowed) {
              allowed = rtl.excludeSet(allowed,0)}
             else break}
           else if (($tmp === "E") || ($tmp === "e")) if (1 in allowed) {
            allowed = {};
            this.ProcessChar();
            if (this.fBuf[this.fPos].charCodeAt() in rtl.createSet(43,45)) this.ProcessChar();
            if (!(this.fBuf[this.fPos].charCodeAt() in rtl.createSet(null,48,57))) this.ErrorFmt(rtl.getResStr(pas.RTLConsts,"SParserInvalidFloat"),pas.System.VarRecs(18,this.fLastTokenStr + this.fBuf[this.fPos]));
          } else break;
          this.ProcessChar();
        };
      };
      if (this.fBuf[this.fPos].charCodeAt() in rtl.createSet(115,83,100,68,99,67)) {
        this.fFloatType = this.fBuf[this.fPos];
        this.GotoToNextChar();
        this.fToken = 5;
      } else this.fFloatType = "\x00";
    };
    this.HandleHexNumber = function () {
      var valid = false;
      this.fLastTokenStr = "$";
      this.GotoToNextChar();
      valid = false;
      while (this.IsHexNum()) {
        valid = true;
        this.ProcessChar();
      };
      if (!valid) this.ErrorFmt(rtl.getResStr(pas.RTLConsts,"SParserInvalidInteger"),pas.System.VarRecs(18,this.fLastTokenStr));
      this.fToken = 4;
    };
    this.HandleQuotedString = function () {
      var Result = "";
      Result = "";
      this.GotoToNextChar();
      while (true) {
        var $tmp = this.fBuf[this.fPos];
        if ($tmp === "\x00") {
          this.ErrorStr(rtl.getResStr(pas.RTLConsts,"SParserUnterminatedString"))}
         else if (($tmp === "\r") || ($tmp === "\n")) {
          this.ErrorStr(rtl.getResStr(pas.RTLConsts,"SParserUnterminatedString"))}
         else if ($tmp === "'") {
          this.GotoToNextChar();
          if (this.fBuf[this.fPos] !== "'") return Result;
        };
        Result = Result + this.fBuf[this.fPos];
        this.GotoToNextChar();
      };
      return Result;
    };
    this.HandleDecimalCharacter = function () {
      var Result = "";
      var i = 0;
      this.GotoToNextChar();
      i = 0;
      while (this.IsNumber() && (i < 65535)) {
        i = ((i * 10) + this.fBuf[this.fPos].charCodeAt()) - 48;
        this.GotoToNextChar();
      };
      if (i > 65535) i = 0;
      Result = String.fromCharCode(i);
      return Result;
    };
    this.HandleString = function () {
      var s = "";
      this.fLastTokenStr = "";
      while (true) {
        var $tmp = this.fBuf[this.fPos];
        if ($tmp === "'") {
          s = this.HandleQuotedString();
          this.fLastTokenStr = this.fLastTokenStr + s;
        } else if ($tmp === "#") {
          this.fLastTokenStr = this.fLastTokenStr + this.HandleDecimalCharacter();
        } else {
          break;
        };
      };
      this.fToken = 3;
    };
    this.HandleMinus = function () {
      this.GotoToNextChar();
      if (this.IsNumber()) {
        this.HandleNumber();
        this.fLastTokenStr = "-" + this.fLastTokenStr;
      } else {
        this.fToken = 6;
        this.fLastTokenStr = "-";
      };
    };
    this.HandleUnknown = function () {
      this.fToken = 0;
      this.fLastTokenStr = this.fBuf[this.fPos];
      this.GotoToNextChar();
    };
    this.GotoToNextChar = function () {
      this.fPos += 1;
      this.CheckLoadBuffer();
    };
    this.Create$1 = function (Stream) {
      this.fStream = Stream;
      this.fBuf = rtl.arraySetLength(this.fBuf,"",4096);
      this.FBufLen = 0;
      this.fPos = 0;
      this.fDeltaPos = 1;
      this.fSourceLine = 1;
      this.fEofReached = false;
      this.fLastTokenStr = "";
      this.fFloatType = "\x00";
      this.fToken = 1;
      this.LoadBuffer();
      this.SkipBOM();
      this.NextToken();
      return this;
    };
    this.Destroy = function () {
      var aCount = 0;
      aCount = this.fLastTokenStr.length * 2;
      this.fStream.SetPosition(this.SourcePos() - aCount);
    };
    this.CheckToken = function (T) {
      if (this.fToken !== T) this.ErrorFmt(rtl.getResStr(pas.RTLConsts,"SParserWrongTokenType"),pas.System.VarRecs(18,this.GetTokenName(T),18,this.GetTokenName(this.fToken)));
    };
    this.CheckTokenSymbol = function (S) {
      this.CheckToken(2);
      if (pas.SysUtils.CompareText(this.fLastTokenStr,S) !== 0) this.ErrorFmt(rtl.getResStr(pas.RTLConsts,"SParserWrongTokenSymbol"),pas.System.VarRecs(18,S,18,this.fLastTokenStr));
    };
    this.Error = function (Ident) {
      this.ErrorStr(Ident);
    };
    this.ErrorFmt = function (Ident, Args) {
      this.ErrorStr(pas.SysUtils.Format(Ident,Args));
    };
    this.ErrorStr = function (Message) {
      throw $mod.EParserError.$create("CreateFmt",[Message + rtl.getResStr(pas.RTLConsts,"SParserLocInfo"),pas.System.VarRecs(0,this.fSourceLine,0,this.fPos + this.fDeltaPos,0,this.SourcePos())]);
    };
    this.HexToBinary = function (Stream) {
      var outbuf = [];
      var b = 0;
      var i = 0;
      outbuf = rtl.arraySetLength(outbuf,0,4096);
      i = 0;
      this.SkipWhitespace();
      while (this.IsHexNum()) {
        b = this.GetHexValue(this.fBuf[this.fPos]) << 4;
        this.GotoToNextChar();
        if (!this.IsHexNum()) this.Error(rtl.getResStr(pas.RTLConsts,"SParserUnterminatedBinValue"));
        b = b | this.GetHexValue(this.fBuf[this.fPos]);
        this.GotoToNextChar();
        outbuf[i] = b;
        i += 1;
        if (i >= 4096) {
          Stream.WriteBuffer(outbuf,i);
          i = 0;
        };
        this.SkipWhitespace();
      };
      if (i > 0) Stream.WriteBuffer(outbuf,i);
      this.NextToken();
    };
    this.NextToken = function () {
      var $Self = this;
      var Result = 0;
      function SetToken(aToken) {
        $Self.fToken = aToken;
        $Self.GotoToNextChar();
      };
      this.SkipWhitespace();
      if (this.fEofReached) {
        this.HandleEof()}
       else {
        var $tmp = this.fBuf[this.fPos];
        if (($tmp === "_") || (($tmp >= "A") && ($tmp <= "Z")) || (($tmp >= "a") && ($tmp <= "z"))) {
          this.HandleAlphaNum()}
         else if ($tmp === "$") {
          this.HandleHexNumber()}
         else if ($tmp === "-") {
          this.HandleMinus()}
         else if (($tmp >= "0") && ($tmp <= "9")) {
          this.HandleNumber()}
         else if (($tmp === "'") || ($tmp === "#")) {
          this.HandleString()}
         else if ($tmp === "[") {
          SetToken(7)}
         else if ($tmp === "(") {
          SetToken(8)}
         else if ($tmp === "<") {
          SetToken(9)}
         else if ($tmp === "{") {
          SetToken(10)}
         else if ($tmp === "]") {
          SetToken(11)}
         else if ($tmp === ")") {
          SetToken(12)}
         else if ($tmp === ">") {
          SetToken(13)}
         else if ($tmp === "}") {
          SetToken(14)}
         else if ($tmp === ",") {
          SetToken(15)}
         else if ($tmp === ".") {
          SetToken(16)}
         else if ($tmp === "=") {
          SetToken(17)}
         else if ($tmp === ":") {
          SetToken(18)}
         else if ($tmp === "+") {
          SetToken(19)}
         else {
          this.HandleUnknown();
        };
      };
      Result = this.fToken;
      return Result;
    };
    this.SourcePos = function () {
      var Result = 0;
      Result = (this.fStream.GetPosition() - this.FBufLen) + this.fPos;
      return Result;
    };
    this.TokenComponentIdent = function () {
      var Result = "";
      if (this.fToken !== 2) this.ErrorFmt(rtl.getResStr(pas.RTLConsts,"SParserExpected"),pas.System.VarRecs(18,this.GetTokenName(2)));
      this.CheckLoadBuffer();
      while (this.fBuf[this.fPos] === ".") {
        this.ProcessChar();
        this.fLastTokenStr = this.fLastTokenStr + this.GetAlphaNum();
      };
      Result = this.fLastTokenStr;
      return Result;
    };
    this.TokenFloat = function () {
      var Result = 0.0;
      var errcode = 0;
      pas.System.val$8(this.fLastTokenStr,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},{get: function () {
          return errcode;
        }, set: function (v) {
          errcode = v;
        }});
      if (errcode !== 0) this.ErrorFmt(rtl.getResStr(pas.RTLConsts,"SParserInvalidFloat"),pas.System.VarRecs(18,this.fLastTokenStr));
      return Result;
    };
    this.TokenInt = function () {
      var Result = 0;
      if (!pas.SysUtils.TryStrToInt64(this.fLastTokenStr,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }})) Result = pas.SysUtils.StrToQWord(this.fLastTokenStr);
      return Result;
    };
    this.TokenString = function () {
      var Result = "";
      var $tmp = this.fToken;
      if ($tmp === 5) {
        if (this.fFloatType !== "\x00") {
          Result = this.fLastTokenStr + this.fFloatType}
         else Result = this.fLastTokenStr}
       else {
        Result = this.fLastTokenStr;
      };
      return Result;
    };
    this.TokenSymbolIs = function (S) {
      var Result = false;
      Result = (this.fToken === 2) && (pas.SysUtils.CompareText(this.fLastTokenStr,S) === 0);
      return Result;
    };
  });
  rtl.createClass(this,"TObjectTextConverter",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FParser = null;
      this.FInput = null;
      this.Foutput = null;
    };
    this.$final = function () {
      this.FParser = undefined;
      this.FInput = undefined;
      this.Foutput = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.WriteDouble = function (e) {
      this.Foutput.WriteBufferData$20(e);
    };
    this.WriteDWord = function (lw) {
      this.Foutput.WriteBufferData$14(lw);
    };
    this.WriteInteger = function (value) {
      if ((value >= -128) && (value <= 127)) {
        this.Foutput.WriteByte(2);
        this.Foutput.WriteByte(value & 255);
      } else if ((value >= -32768) && (value <= 32767)) {
        this.Foutput.WriteByte(3);
        this.WriteWord(value & 65535);
      } else if ((value >= -2147483648) && (value <= 2147483647)) {
        this.Foutput.WriteByte(4);
        this.WriteDWord(value >>> 0);
      } else {
        this.Foutput.WriteByte($mod.TValueType.vaNativeInt);
        this.WriteQWord(value);
      };
    };
    this.WriteQWord = function (q) {
      this.Foutput.WriteBufferData$16(q);
    };
    this.WriteString = function (s) {
      var i = 0;
      var size = 0;
      if (s.length > 255) {
        size = 255}
       else size = s.length;
      this.Foutput.WriteByte(size);
      for (var $l = 1, $end = s.length; $l <= $end; $l++) {
        i = $l;
        this.Foutput.WriteBufferData$4(s.charAt(i - 1));
      };
    };
    this.WriteWord = function (w) {
      this.Foutput.WriteBufferData$12(w);
    };
    this.WriteWString = function (s) {
      var i = 0;
      this.WriteDWord(s.length);
      for (var $l = 1, $end = s.length; $l <= $end; $l++) {
        i = $l;
        this.Foutput.WriteBufferData$4(s.charAt(i - 1));
      };
    };
    this.ProcessObject = function () {
      var Flags = 0;
      var ObjectName = "";
      var ObjectType = "";
      var ChildPos = 0;
      if (this.FParser.TokenSymbolIs("OBJECT")) {
        Flags = 0}
       else {
        if (this.FParser.TokenSymbolIs("INHERITED")) {
          Flags = 1}
         else {
          this.FParser.CheckTokenSymbol("INLINE");
          Flags = 4;
        };
      };
      this.FParser.NextToken();
      this.FParser.CheckToken(2);
      ObjectName = "";
      ObjectType = this.FParser.TokenString();
      this.FParser.NextToken();
      if (this.FParser.fToken === 18) {
        this.FParser.NextToken();
        this.FParser.CheckToken(2);
        ObjectName = ObjectType;
        ObjectType = this.FParser.TokenString();
        this.FParser.NextToken();
        if (this.FParser.fToken === 7) {
          this.FParser.NextToken();
          ChildPos = this.FParser.TokenInt();
          this.FParser.NextToken();
          this.FParser.CheckToken(11);
          this.FParser.NextToken();
          Flags = Flags | 2;
        };
      };
      if (Flags !== 0) {
        this.Foutput.WriteByte(0xf0 | Flags);
        if ((Flags & 2) !== 0) this.WriteInteger(ChildPos);
      };
      this.WriteString(ObjectType);
      this.WriteString(ObjectName);
      while (!(this.FParser.TokenSymbolIs("END") || this.FParser.TokenSymbolIs("OBJECT") || this.FParser.TokenSymbolIs("INHERITED") || this.FParser.TokenSymbolIs("INLINE"))) this.ProcessProperty();
      this.Foutput.WriteByte(0);
      while (!this.FParser.TokenSymbolIs("END")) this.ProcessObject();
      this.FParser.NextToken();
      this.Foutput.WriteByte(0);
    };
    this.ProcessProperty = function () {
      var name = "";
      this.FParser.CheckToken(2);
      name = this.FParser.TokenString();
      while (true) {
        this.FParser.NextToken();
        if (this.FParser.fToken !== 16) break;
        this.FParser.NextToken();
        this.FParser.CheckToken(2);
        name = name + "." + this.FParser.TokenString();
      };
      this.WriteString(name);
      this.FParser.CheckToken(17);
      this.FParser.NextToken();
      this.ProcessValue();
    };
    this.ProcessValue = function () {
      var flt = 0.0;
      var stream = null;
      var $tmp = this.FParser.fToken;
      if ($tmp === 4) {
        this.WriteInteger(this.FParser.TokenInt());
        this.FParser.NextToken();
      } else if ($tmp === 5) {
        this.Foutput.WriteByte($mod.TValueType.vaDouble);
        flt = this.FParser.TokenFloat();
        this.WriteDouble(flt);
        this.FParser.NextToken();
      } else if ($tmp === 3) {
        this.ProcessWideString("")}
       else if ($tmp === 2) {
        if (pas.SysUtils.CompareText(this.FParser.TokenString(),"True") === 0) {
          this.Foutput.WriteByte(9)}
         else if (pas.SysUtils.CompareText(this.FParser.TokenString(),"False") === 0) {
          this.Foutput.WriteByte(8)}
         else if (pas.SysUtils.CompareText(this.FParser.TokenString(),"nil") === 0) {
          this.Foutput.WriteByte(12)}
         else {
          this.Foutput.WriteByte(7);
          this.WriteString(this.FParser.TokenComponentIdent());
        };
        this.FParser.NextToken();
      } else if ($tmp === 7) {
        this.FParser.NextToken();
        this.Foutput.WriteByte(11);
        if (this.FParser.fToken !== 11) while (true) {
          this.FParser.CheckToken(2);
          this.WriteString(this.FParser.TokenString());
          this.FParser.NextToken();
          if (this.FParser.fToken === 11) break;
          this.FParser.CheckToken(15);
          this.FParser.NextToken();
        };
        this.Foutput.WriteByte(0);
        this.FParser.NextToken();
      } else if ($tmp === 8) {
        this.FParser.NextToken();
        this.Foutput.WriteByte(1);
        while (this.FParser.fToken !== 12) this.ProcessValue();
        this.Foutput.WriteByte(0);
        this.FParser.NextToken();
      } else if ($tmp === 9) {
        this.FParser.NextToken();
        this.Foutput.WriteByte(13);
        while (this.FParser.fToken !== 13) {
          this.FParser.CheckTokenSymbol("item");
          this.FParser.NextToken();
          this.Foutput.WriteByte(1);
          while (!this.FParser.TokenSymbolIs("end")) this.ProcessProperty();
          this.FParser.NextToken();
          this.Foutput.WriteByte(0);
        };
        this.Foutput.WriteByte(0);
        this.FParser.NextToken();
      } else if ($tmp === 10) {
        this.Foutput.WriteByte(10);
        stream = $mod.TBytesStream.$create("Create");
        try {
          this.FParser.HexToBinary(stream);
          this.WriteDWord(stream.GetSize());
          this.Foutput.WriteBuffer(stream.GetBytes(),stream.GetSize());
        } finally {
          stream = rtl.freeLoc(stream);
        };
        this.FParser.NextToken();
      } else {
        this.FParser.Error(rtl.getResStr(pas.RTLConsts,"SParserInvalidProperty"));
      };
    };
    this.ProcessWideString = function (left) {
      var ws = "";
      ws = left + this.FParser.TokenString();
      while (this.FParser.NextToken() === 19) {
        this.FParser.NextToken();
        if (!(this.FParser.fToken === 3)) this.FParser.CheckToken(3);
        ws = ws + this.FParser.TokenString();
      };
      this.Foutput.WriteByte($mod.TValueType.vaString);
      this.WriteWString(ws);
    };
    this.ObjectTextToBinary = function (aInput, aOutput) {
      this.FInput = aInput;
      this.Foutput = aOutput;
      this.Execute();
    };
    this.Execute = function () {
      if (!(this.FInput != null)) throw $mod.EReadError.$create("Create$1",["Missing input stream"]);
      if (!(this.Foutput != null)) throw $mod.EReadError.$create("Create$1",["Missing output stream"]);
      this.FParser = $mod.TParser.$create("Create$1",[this.FInput]);
      try {
        this.Foutput.WriteBufferData$14(809914452);
        this.ProcessObject();
      } finally {
        rtl.free(this,"FParser");
      };
    };
  });
  rtl.recNewT(this,"TIdentMapEntry",function () {
    this.Value = 0;
    this.Name = "";
    this.$eq = function (b) {
      return (this.Value === b.Value) && (this.Name === b.Name);
    };
    this.$assign = function (s) {
      this.Value = s.Value;
      this.Name = s.Name;
      return this;
    };
  });
  this.$rtti.$ProcVar("TInitComponentHandler",{procsig: rtl.newTIProcSig([["Instance",this.$rtti["TComponent"]],["RootAncestor",pas.System.$rtti["TClass"]]],rtl.boolean)});
  this.RegisterInitComponentHandler = function (ComponentClass, Handler) {
    var I = 0;
    var H = null;
    if ($impl.InitHandlerList === null) $impl.InitHandlerList = $mod.TList.$create("Create$1");
    H = $impl.TInitHandler.$create("Create");
    H.AClass = ComponentClass;
    H.AHandler = Handler;
    try {
      var $with = $impl.InitHandlerList;
      I = 0;
      while ((I < $with.GetCount()) && !H.AClass.InheritsFrom(rtl.getObject($with.Get(I)).AClass)) I += 1;
      if ((I < $with.GetCount()) && (rtl.getObject($with.Get(I)).AClass === H.AClass)) {
        rtl.getObject($with.Get(I)).AHandler = Handler;
        H = rtl.freeLoc(H);
      } else $impl.InitHandlerList.Insert(I,H);
    } catch ($e) {
      H = rtl.freeLoc(H);
      throw $e;
    };
  };
  this.GetClass = function (AClassName) {
    var Result = null;
    Result = null;
    if (AClassName === "") return Result;
    if (!$impl.ClassList.hasOwnProperty(AClassName)) return Result;
    Result = rtl.getObject($impl.ClassList[AClassName]);
    return Result;
  };
  this.FindGlobalComponent = function (Name) {
    var Result = null;
    var i = 0;
    Result = null;
    if ($impl.FindGlobalComponentList != null) {
      for (var $l = $impl.FindGlobalComponentList.FCount - 1; $l >= 0; $l--) {
        i = $l;
        Result = $impl.FindGlobalComponentList.Get(i)(Name);
        if (Result != null) break;
      };
    };
    return Result;
  };
  this.FindNestedComponent = function (Root, APath, CStyle) {
    var Result = null;
    function GetNextName() {
      var Result = "";
      var P = 0;
      var CM = false;
      P = pas.System.Pos(".",APath);
      CM = false;
      if (P === 0) {
        if (CStyle) {
          P = pas.System.Pos("->",APath);
          CM = P !== 0;
        };
        if (P === 0) P = APath.length + 1;
      };
      Result = pas.System.Copy(APath,1,P - 1);
      pas.System.Delete({get: function () {
          return APath;
        }, set: function (v) {
          APath = v;
        }},1,P + (CM + 0));
      return Result;
    };
    var C = null;
    var S = "";
    if (APath === "") {
      Result = null}
     else {
      Result = Root;
      while ((APath !== "") && (Result !== null)) {
        C = Result;
        S = pas.SysUtils.UpperCase(GetNextName());
        Result = C.FindComponent(S);
        if ((Result === null) && (S === "OWNER")) Result = C;
      };
    };
    return Result;
  };
  this.RemoveFixupReferences = function (Root, RootName) {
    if ($impl.NeedResolving === null) return;
    $impl.VisitResolveList($impl.TRemoveReferenceVisitor.$create("Create$1",[Root,RootName]));
  };
  this.RegisterIntegerConsts = function (IntegerType, IdentToIntFn, IntToIdentFn) {
    if (!($impl.IntConstList != null)) $impl.IntConstList = $mod.TFPList.$create("Create");
    $impl.IntConstList.Add($impl.TIntConst.$create("Create$1",[IntegerType,IdentToIntFn,IntToIdentFn]));
  };
  this.IdentToInt = function (Ident, Int, map) {
    var Result = false;
    var i = 0;
    for (var $l = 0, $end = rtl.length(map) - 1; $l <= $end; $l++) {
      i = $l;
      if (pas.SysUtils.CompareText(map[i].Name,Ident) === 0) {
        Int.set(map[i].Value);
        return true;
      };
    };
    Result = false;
    return Result;
  };
  this.IntToIdent = function (Int, Ident, map) {
    var Result = false;
    var i = 0;
    for (var $l = 0, $end = rtl.length(map) - 1; $l <= $end; $l++) {
      i = $l;
      if (map[i].Value === Int) {
        Ident.set(map[i].Name);
        return true;
      };
    };
    Result = false;
    return Result;
  };
  this.FindClass = function (AClassName) {
    var Result = null;
    Result = $mod.GetClass(AClassName);
    if (!(Result != null)) throw $mod.EClassNotFound.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SClassNotFound"),pas.System.VarRecs(18,AClassName)]);
    return Result;
  };
  this.ObjectTextToBinary = function (aInput, aOutput) {
    var Conv = null;
    Conv = $mod.TObjectTextConverter.$create("Create");
    try {
      Conv.ObjectTextToBinary(aInput,aOutput);
    } finally {
      Conv = rtl.freeLoc(Conv);
    };
  };
  this.StringToBuffer = function (aString, aLen) {
    var Result = null;
    var I = 0;
    Result = new ArrayBuffer(aLen * 2);
    var $with = new Uint16Array(Result);
    for (var $l = 0, $end = aLen - 1; $l <= $end; $l++) {
      I = $l;
      $with[I] = aString.charCodeAt(I);
    };
    return Result;
  };
  this.vaExtended = 5;
  this.vaWString = 6;
  this.vaInt64 = 16;
  $mod.$implcode = function () {
    $impl.GlobalLoaded = null;
    $impl.IntConstList = null;
    rtl.createClass($impl,"TIntConst",pas.System.TObject,function () {
      this.$init = function () {
        pas.System.TObject.$init.call(this);
        this.IntegerType = null;
        this.IdentToIntFn = null;
        this.IntToIdentFn = null;
      };
      this.$final = function () {
        this.IdentToIntFn = undefined;
        this.IntToIdentFn = undefined;
        pas.System.TObject.$final.call(this);
      };
      this.Create$1 = function (AIntegerType, AIdentToInt, AIntToIdent) {
        this.IntegerType = AIntegerType;
        this.IdentToIntFn = AIdentToInt;
        this.IntToIdentFn = AIntToIdent;
        return this;
      };
    });
    $impl.GlobalIdentToInt = function (Ident, Int) {
      var Result = false;
      var i = 0;
      Result = false;
      if (!($impl.IntConstList != null)) return Result;
      var $with = $impl.IntConstList;
      for (var $l = 0, $end = $with.FCount - 1; $l <= $end; $l++) {
        i = $l;
        if (rtl.getObject($with.Get(i)).IdentToIntFn(Ident,Int)) return true;
      };
      return Result;
    };
    $impl.TMSGrow = 4096;
    $impl.FilerSignatureInt = 809914452;
    rtl.createClass($impl,"TUnresolvedReference",pas.simplelinkedlist.TLinkedListItem,function () {
      this.$init = function () {
        pas.simplelinkedlist.TLinkedListItem.$init.call(this);
        this.FRoot = null;
        this.FPropInfo = null;
        this.FGlobal = "";
        this.FRelative = "";
      };
      this.$final = function () {
        this.FRoot = undefined;
        this.FPropInfo = undefined;
        pas.simplelinkedlist.TLinkedListItem.$final.call(this);
      };
      this.Resolve = function (Instance) {
        var Result = false;
        var C = null;
        C = $mod.FindGlobalComponent(this.FGlobal);
        Result = C !== null;
        if (Result) {
          C = $mod.FindNestedComponent(C,this.FRelative,true);
          Result = C !== null;
          if (Result) pas.TypInfo.SetObjectProp$1(Instance,this.FPropInfo,C);
        };
        return Result;
      };
      this.RootMatches = function (ARoot) {
        var Result = false;
        Result = (ARoot === null) || (ARoot === this.FRoot);
        return Result;
      };
      this.NextRef = function () {
        var Result = null;
        Result = this.Next;
        return Result;
      };
    });
    rtl.createClass($impl,"TLocalUnResolvedReference",$impl.TUnresolvedReference,function () {
      this.$init = function () {
        $impl.TUnresolvedReference.$init.call(this);
        this.Finstance = null;
      };
      this.$final = function () {
        this.Finstance = undefined;
        $impl.TUnresolvedReference.$final.call(this);
      };
      var $r = this.$rtti;
      $r.addField("Finstance",$mod.$rtti["TPersistent"]);
    });
    rtl.createClass($impl,"TUnResolvedInstance",pas.simplelinkedlist.TLinkedListItem,function () {
      this.$init = function () {
        pas.simplelinkedlist.TLinkedListItem.$init.call(this);
        this.Instance = null;
        this.FUnresolved = null;
      };
      this.$final = function () {
        this.Instance = undefined;
        this.FUnresolved = undefined;
        pas.simplelinkedlist.TLinkedListItem.$final.call(this);
      };
      this.Destroy = function () {
        rtl.free(this,"FUnresolved");
        pas.System.TObject.Destroy.call(this);
      };
      this.AddReference = function (ARoot, APropInfo, AGlobal, ARelative) {
        var Result = null;
        if (this.FUnresolved === null) this.FUnresolved = pas.simplelinkedlist.TLinkedList.$create("Create$1",[$impl.TUnresolvedReference]);
        Result = rtl.as(this.FUnresolved.Add(),$impl.TUnresolvedReference);
        Result.FGlobal = AGlobal;
        Result.FRelative = ARelative;
        Result.FPropInfo = APropInfo;
        Result.FRoot = ARoot;
        return Result;
      };
      this.RootUnresolved = function () {
        var Result = null;
        Result = null;
        if (this.FUnresolved != null) Result = this.FUnresolved.FRoot;
        return Result;
      };
      this.ResolveReferences = function () {
        var Result = false;
        var R = null;
        var RN = null;
        R = this.RootUnresolved();
        while (R !== null) {
          RN = R.NextRef();
          if (R.Resolve(this.Instance)) this.FUnresolved.RemoveItem(R,true);
          R = RN;
        };
        Result = this.RootUnresolved() === null;
        return Result;
      };
    });
    rtl.createClass($impl,"TBuildListVisitor",pas.simplelinkedlist.TLinkedListVisitor,function () {
      this.$init = function () {
        pas.simplelinkedlist.TLinkedListVisitor.$init.call(this);
        this.List = null;
      };
      this.$final = function () {
        this.List = undefined;
        pas.simplelinkedlist.TLinkedListVisitor.$final.call(this);
      };
      this.Add = function (Item) {
        if (this.List === null) this.List = $mod.TFPList.$create("Create");
        this.List.Add(Item);
      };
      this.Destroy = function () {
        var I = 0;
        if (this.List != null) for (var $l = 0, $end = this.List.FCount - 1; $l <= $end; $l++) {
          I = $l;
          $impl.NeedResolving.RemoveItem(rtl.getObject(this.List.Get(I)),true);
        };
        pas.SysUtils.FreeAndNil({p: this, get: function () {
            return this.p.List;
          }, set: function (v) {
            this.p.List = v;
          }});
        pas.System.TObject.Destroy.call(this);
      };
    });
    rtl.createClass($impl,"TResolveReferenceVisitor",$impl.TBuildListVisitor,function () {
      this.Visit = function (Item) {
        var Result = false;
        if (Item.ResolveReferences()) this.Add(Item);
        Result = true;
        return Result;
      };
      var $r = this.$rtti;
      $r.addMethod("Visit",1,[["Item",pas.simplelinkedlist.$rtti["TLinkedListItem"]]],rtl.boolean);
    });
    rtl.createClass($impl,"TRemoveReferenceVisitor",$impl.TBuildListVisitor,function () {
      this.$init = function () {
        $impl.TBuildListVisitor.$init.call(this);
        this.FRef = "";
        this.FRoot = null;
      };
      this.$final = function () {
        this.FRoot = undefined;
        $impl.TBuildListVisitor.$final.call(this);
      };
      this.Create$1 = function (ARoot, ARef) {
        this.FRoot = ARoot;
        this.FRef = pas.SysUtils.UpperCase(ARef);
        return this;
      };
      this.Visit = function (Item) {
        var Result = false;
        var I = 0;
        var UI = null;
        var R = null;
        var L = null;
        UI = Item;
        R = UI.RootUnresolved();
        L = null;
        try {
          while (R !== null) {
            if (R.RootMatches(this.FRoot) && ((this.FRef === "") || (this.FRef === pas.SysUtils.UpperCase(R.FGlobal)))) {
              if (!(L != null)) L = $mod.TFPList.$create("Create");
              L.Add(R);
            };
            R = R.NextRef();
          };
          if (L != null) {
            for (var $l = 0, $end = L.FCount - 1; $l <= $end; $l++) {
              I = $l;
              UI.FUnresolved.RemoveItem(rtl.getObject(L.Get(I)),true);
            };
          };
          if (UI.FUnresolved.FRoot === null) {
            if (this.List === null) this.List = $mod.TFPList.$create("Create");
            this.List.Add(UI);
          };
        } finally {
          L = rtl.freeLoc(L);
        };
        Result = true;
        return Result;
      };
    });
    $impl.NeedResolving = null;
    $impl.FindUnresolvedInstance = function (AInstance) {
      var Result = null;
      Result = null;
      if ($impl.NeedResolving != null) {
        Result = $impl.NeedResolving.FRoot;
        while ((Result !== null) && (Result.Instance !== AInstance)) Result = Result.Next;
      };
      return Result;
    };
    $impl.AddtoResolveList = function (AInstance) {
      var Result = null;
      Result = $impl.FindUnresolvedInstance(AInstance);
      if (Result === null) {
        if (!($impl.NeedResolving != null)) $impl.NeedResolving = pas.simplelinkedlist.TLinkedList.$create("Create$1",[$impl.TUnResolvedInstance]);
        Result = rtl.as($impl.NeedResolving.Add(),$impl.TUnResolvedInstance);
        Result.Instance = AInstance;
      };
      return Result;
    };
    $impl.VisitResolveList = function (V) {
      try {
        $impl.NeedResolving.ForEach(V);
      } finally {
        pas.SysUtils.FreeAndNil({get: function () {
            return V;
          }, set: function (v) {
            V = v;
          }});
      };
    };
    $impl.GlobalFixupReferences = function () {
      if ($impl.NeedResolving === null) return;
      $impl.VisitResolveList($impl.TResolveReferenceVisitor.$create("Create"));
    };
    $impl.ClassList = null;
    $impl.InitHandlerList = null;
    $impl.FindGlobalComponentList = null;
    rtl.createClass($impl,"TInitHandler",pas.System.TObject,function () {
      this.$init = function () {
        pas.System.TObject.$init.call(this);
        this.AHandler = null;
        this.AClass = null;
      };
      this.$final = function () {
        this.AHandler = undefined;
        this.AClass = undefined;
        pas.System.TObject.$final.call(this);
      };
      var $r = this.$rtti;
      $r.addField("AHandler",$mod.$rtti["TInitComponentHandler"]);
      $r.addField("AClass",$mod.$rtti["TComponentClass"]);
    });
    $impl.ParseBufSize = 4096;
    $impl.TokNames = ["?","EOF","Symbol","String","Integer","Float","-","[","(","<","{","]",")",">","}",",",".","=",":","+"];
    $mod.$resourcestrings = {SReadError: {org: "Could not read data from stream"}, SWriteError: {org: "Could not write data to stream"}, SMemoryStreamError: {org: "Could not allocate memory"}};
  };
  $mod.$init = function () {
    $impl.ClassList = new Object();
  };
},["simplelinkedlist"]);
rtl.module("Web",["System","Types","JS"],function () {
  "use strict";
  var $mod = this;
});
rtl.module("Graphics",["System","Classes","SysUtils","Types","Web"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.$rtti.$Int("TFontCharSet",{minvalue: 0, maxvalue: 255, ordtype: 3});
  this.TFontStyle = {"0": "fsBold", fsBold: 0, "1": "fsItalic", fsItalic: 1, "2": "fsUnderline", fsUnderline: 2, "3": "fsStrikeOut", fsStrikeOut: 3};
  this.$rtti.$Enum("TFontStyle",{minvalue: 0, maxvalue: 3, ordtype: 1, enumtype: this.TFontStyle});
  this.$rtti.$Set("TFontStyles",{comptype: this.$rtti["TFontStyle"]});
  this.TTextLayout = {"0": "tlTop", tlTop: 0, "1": "tlCenter", tlCenter: 1, "2": "tlBottom", tlBottom: 2};
  this.$rtti.$Enum("TTextLayout",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TTextLayout});
  this.TPenStyle = {"0": "psSolid", psSolid: 0, "1": "psDash", psDash: 1, "2": "psDot", psDot: 2, "3": "psDashDot", psDashDot: 3, "4": "psDashDotDot", psDashDotDot: 4, "5": "psInsideFrame", psInsideFrame: 5, "6": "psPattern", psPattern: 6, "7": "psClear", psClear: 7};
  this.$rtti.$Enum("TPenStyle",{minvalue: 0, maxvalue: 7, ordtype: 1, enumtype: this.TPenStyle});
  this.TBrushStyle = {"0": "bsSolid", bsSolid: 0, "1": "bsClear", bsClear: 1, "2": "bsHorizontal", bsHorizontal: 2, "3": "bsVertical", bsVertical: 3, "4": "bsFDiagonal", bsFDiagonal: 4, "5": "bsBDiagonal", bsBDiagonal: 5, "6": "bsCross", bsCross: 6, "7": "bsDiagCross", bsDiagCross: 7, "8": "bsImage", bsImage: 8, "9": "bsPattern", bsPattern: 9};
  this.$rtti.$Enum("TBrushStyle",{minvalue: 0, maxvalue: 9, ordtype: 1, enumtype: this.TBrushStyle});
  rtl.createClass(this,"TFont",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FCharSet = 0;
      this.FColor = 0;
      this.FName = "";
      this.FSize = 0;
      this.FStyle = {};
      this.FUpdateCount = 0;
      this.FOnChange = null;
    };
    this.$final = function () {
      this.FStyle = undefined;
      this.FOnChange = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    this.GetHeight = function () {
      var Result = 0;
      Result = Math.round((-this.FSize * 72) / 96);
      return Result;
    };
    this.SetCharSet = function (AValue) {
      if (this.FCharSet !== AValue) {
        this.FCharSet = AValue;
        this.Changed();
      };
    };
    this.SetColor = function (AValue) {
      if (this.FColor !== AValue) {
        this.FColor = AValue;
        this.Changed();
      };
    };
    this.SetHeight = function (AValue) {
      this.SetSize(Math.round((-AValue * 96) / 72));
    };
    this.SetName = function (AValue) {
      if (this.FName !== AValue) {
        this.FName = AValue;
        this.Changed();
      };
    };
    this.SetSize = function (AValue) {
      if (this.FSize !== AValue) {
        this.FSize = AValue;
        this.Changed();
      };
    };
    this.SetStyle = function (AValue) {
      if (rtl.neSet(this.FStyle,AValue)) {
        this.FStyle = rtl.refSet(AValue);
        this.Changed();
      };
    };
    this.Changed = function () {
      if ((this.FUpdateCount === 0) && (this.FOnChange != null)) {
        this.FOnChange(this);
      };
    };
    this.Create$1 = function () {
      pas.System.TObject.Create.call(this);
      this.FColor = 0;
      this.FName = $mod.ffSans;
      this.FSize = 10;
      this.FStyle = {};
      this.FUpdateCount = 0;
      return this;
    };
    this.Assign = function (Source) {
      var VFont = null;
      if ((Source != null) && $mod.TFont.isPrototypeOf(Source)) {
        this.BeginUpdate();
        try {
          VFont = Source;
          this.FCharSet = VFont.FCharSet;
          this.FColor = VFont.FColor;
          this.FName = VFont.FName;
          this.FSize = VFont.FSize;
          this.FStyle = rtl.refSet(VFont.FStyle);
        } finally {
          this.EndUpdate();
        };
      } else {
        pas.Classes.TPersistent.Assign.call(this,Source);
      };
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) {
        this.FUpdateCount -= 1;
        if (this.FUpdateCount === 0) {
          this.Changed();
        };
      };
    };
    this.IsEqual = function (AFont) {
      var Result = false;
      if (AFont != null) {
        if ((this.FCharSet !== AFont.FCharSet) || (this.FColor !== AFont.FColor) || (this.FName !== AFont.FName) || (this.FSize !== AFont.FSize) || rtl.neSet(this.FStyle,AFont.FStyle)) {
          Result = false;
        } else {
          Result = true;
        };
      } else {
        Result = false;
      };
      return Result;
    };
    this.TextExtent = function (AText) {
      var Result = pas.Types.TSize.$new();
      Result.$assign($mod.JSMeasureText(AText,this.FName,this.FSize,0));
      return Result;
    };
    this.TextHeight = function (AText) {
      var Result = 0;
      Result = this.TextExtent(AText).cy;
      return Result;
    };
    this.TextWidth = function (AText) {
      var Result = 0;
      Result = this.TextExtent(AText).cx;
      return Result;
    };
    var $r = this.$rtti;
    $r.addProperty("CharSet",2,$mod.$rtti["TFontCharSet"],"FCharSet","SetCharSet");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Height",3,rtl.nativeint,"GetHeight","SetHeight");
    $r.addProperty("Name",2,rtl.string,"FName","SetName");
    $r.addProperty("Size",2,rtl.nativeint,"FSize","SetSize");
    $r.addProperty("Style",2,$mod.$rtti["TFontStyles"],"FStyle","SetStyle");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
  });
  rtl.createClass(this,"TPen",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FColor = 0;
      this.FStyle = 0;
      this.FWidth = 0;
      this.FUpdateCount = 0;
      this.FOnChange = null;
    };
    this.$final = function () {
      this.FOnChange = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    this.SetColor = function (AValue) {
      if (this.FColor !== AValue) {
        this.FColor = AValue;
        this.Changed();
      };
    };
    this.SetStyle = function (AValue) {
      if (this.FStyle !== AValue) {
        this.FStyle = AValue;
        this.Changed();
      };
    };
    this.SetWidth = function (AValue) {
      if (this.FWidth !== AValue) {
        this.FWidth = AValue;
        this.Changed();
      };
    };
    this.Changed = function () {
      if ((this.FUpdateCount === 0) && (this.FOnChange != null)) {
        this.FOnChange(this);
      };
    };
    this.Assign = function (Source) {
      var VPen = null;
      if ((Source != null) && $mod.TPen.isPrototypeOf(Source)) {
        this.BeginUpdate();
        try {
          VPen = Source;
          this.FColor = VPen.FColor;
          this.FStyle = VPen.FStyle;
          this.FWidth = VPen.FWidth;
        } finally {
          this.EndUpdate();
        };
      } else {
        pas.Classes.TPersistent.Assign.call(this,Source);
      };
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) {
        this.FUpdateCount -= 1;
        if (this.FUpdateCount === 0) {
          this.Changed();
        };
      };
    };
    var $r = this.$rtti;
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Style",2,$mod.$rtti["TPenStyle"],"FStyle","SetStyle");
    $r.addProperty("Width",2,rtl.nativeint,"FWidth","SetWidth");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
  });
  rtl.createClass(this,"TBrush",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FColor = 0;
      this.FStyle = 0;
      this.FUpdateCount = 0;
      this.FOnChange = null;
    };
    this.$final = function () {
      this.FOnChange = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    this.SetColor = function (AValue) {
      if (this.FColor !== AValue) {
        this.FColor = AValue;
        this.Changed();
      };
    };
    this.SetStyle = function (AValue) {
      if (this.FStyle === AValue) {
        this.FStyle = AValue;
        this.Changed();
      };
    };
    this.Changed = function () {
      if ((this.FUpdateCount === 0) && (this.FOnChange != null)) {
        this.FOnChange(this);
      };
    };
    this.Assign = function (Source) {
      var VBrush = null;
      if ((Source != null) && $mod.TBrush.isPrototypeOf(Source)) {
        this.BeginUpdate();
        try {
          VBrush = Source;
          this.FColor = VBrush.FColor;
          this.FStyle = VBrush.FStyle;
        } finally {
          this.EndUpdate();
        };
      } else {
        pas.Classes.TPersistent.Assign.call(this,Source);
      };
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) {
        this.FUpdateCount -= 1;
        if (this.FUpdateCount === 0) {
          this.Changed();
        };
      };
    };
    var $r = this.$rtti;
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Style",2,$mod.$rtti["TBrushStyle"],"FStyle","SetStyle");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
  });
  rtl.createClass(this,"TPicture",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FData = "";
      this.FUpdateCount = 0;
      this.FOnChange = null;
    };
    this.$final = function () {
      this.FOnChange = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    this.SetData = function (AValue) {
      if (this.FData !== AValue) {
        this.FData = AValue;
        this.Changed();
      };
    };
    this.Changed = function () {
      if ((this.FUpdateCount === 0) && (this.FOnChange != null)) {
        this.FOnChange(this);
      };
    };
    this.Create$1 = function () {
      this.FData = "";
      this.FUpdateCount = 0;
      this.FOnChange = null;
      return this;
    };
    this.Assign = function (Source) {
      var VPicture = null;
      if ((Source != null) && $mod.TPicture.isPrototypeOf(Source)) {
        this.BeginUpdate();
        try {
          VPicture = Source;
          this.FData = VPicture.FData;
        } finally {
          this.EndUpdate();
        };
      } else {
        pas.Classes.TPersistent.Assign.call(this,Source);
      };
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) {
        this.FUpdateCount -= 1;
        if (this.FUpdateCount === 0) {
          this.Changed();
        };
      };
    };
    var $r = this.$rtti;
    $r.addProperty("Data",2,rtl.string,"FData","SetData");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
  });
  rtl.createClass(this,"TCanvas",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FBrush = null;
      this.FFont = null;
      this.FPen = null;
      this.FOnChange = null;
    };
    this.$final = function () {
      this.FBrush = undefined;
      this.FFont = undefined;
      this.FPen = undefined;
      this.FOnChange = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    this.Destroy = function () {
      this.FBrush.$destroy("Destroy");
      this.FFont.$destroy("Destroy");
      this.FPen.$destroy("Destroy");
      this.FBrush = null;
      this.FFont = null;
      this.FPen = null;
      pas.System.TObject.Destroy.call(this);
    };
    var $r = this.$rtti;
    $r.addProperty("Brush",0,$mod.$rtti["TBrush"],"FBrush","FBrush");
    $r.addProperty("Font",0,$mod.$rtti["TFont"],"FFont","FFont");
    $r.addProperty("Pen",0,$mod.$rtti["TPen"],"FPen","FPen");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
  });
  this.clBlack = 0x0;
  this.clMaroon = 0x80;
  this.clGreen = 0x8000;
  this.clOlive = 0x8080;
  this.clNavy = 0x800000;
  this.clPurple = 0x800080;
  this.clTeal = 0x808000;
  this.clGray = 0x808080;
  this.clSilver = 0xC0C0C0;
  this.clRed = 0xFF;
  this.clLime = 0xFF00;
  this.clYellow = 0xFFFF;
  this.clBlue = 0xFF0000;
  this.clFuchsia = 0xFF00FF;
  this.clAqua = 0xFFFF00;
  this.clWhite = 0xFFFFFF;
  this.clMoneyGreen = 0xC0DCC0;
  this.clSkyBlue = 0xF0CAA6;
  this.clCream = 0xF0FBFF;
  this.clMedGray = 0xA4A0A0;
  this.clNone = 0x1FFFFFFF;
  this.clDefault = 0x20000000;
  this.clBase = 0x80000000;
  this.clScrollBar = -2147483648 + 0;
  this.clBackground = -2147483648 + 1;
  this.clActiveCaption = -2147483648 + 2;
  this.clInactiveCaption = -2147483648 + 3;
  this.clMenu = -2147483648 + 4;
  this.clWindow = -2147483648 + 5;
  this.clWindowFrame = -2147483648 + 6;
  this.clMenuText = -2147483648 + 7;
  this.clWindowText = -2147483648 + 8;
  this.clCaptionText = -2147483648 + 9;
  this.clActiveBorder = -2147483648 + 10;
  this.clInactiveBorder = -2147483648 + 11;
  this.clAppWorkspace = -2147483648 + 12;
  this.clHighlight = -2147483648 + 13;
  this.clHighlightText = -2147483648 + 14;
  this.clBtnFace = -2147483648 + 15;
  this.clBtnShadow = -2147483648 + 16;
  this.clGrayText = -2147483648 + 17;
  this.clBtnText = -2147483648 + 18;
  this.clInactiveCaptionText = -2147483648 + 19;
  this.clBtnHighlight = -2147483648 + 20;
  this.cl3DDkShadow = -2147483648 + 21;
  this.cl3DLight = -2147483648 + 22;
  this.clInfoText = -2147483648 + 23;
  this.clInfoBk = -2147483648 + 24;
  this.ffSans = '"Arial Narrow", Arial, "Helvetica Condensed", Helvetica, sans-serif';
  this.JSColor = function (AColor, AAlpha) {
    var Result = "";
    var R = 0;
    var G = 0;
    var B = 0;
    var A = 0;
    A = AAlpha;
    var $tmp = AColor;
    if ($tmp === -2147483648) {
      Result = "Scrollbar"}
     else if ($tmp === -2147483647) {
      Result = "Background"}
     else if ($tmp === -2147483646) {
      Result = "ActiveCaption"}
     else if ($tmp === -2147483645) {
      Result = "InactiveCaption"}
     else if ($tmp === -2147483644) {
      Result = "Menu"}
     else if ($tmp === -2147483643) {
      Result = "Window"}
     else if ($tmp === -2147483642) {
      Result = "WindowFrame"}
     else if ($tmp === -2147483641) {
      Result = "MenuText"}
     else if ($tmp === -2147483640) {
      Result = "WindowText"}
     else if ($tmp === -2147483639) {
      Result = "CaptionText"}
     else if ($tmp === -2147483638) {
      Result = "ActiveBorder"}
     else if ($tmp === -2147483637) {
      Result = "InactiveBorder"}
     else if ($tmp === -2147483636) {
      Result = "AppWorkspace"}
     else if ($tmp === -2147483635) {
      Result = "Highlight"}
     else if ($tmp === -2147483634) {
      Result = "HighlightText"}
     else if ($tmp === -2147483633) {
      Result = "ButtonFace"}
     else if ($tmp === -2147483632) {
      Result = "ButtonShadow"}
     else if ($tmp === -2147483631) {
      Result = "GrayText"}
     else if ($tmp === -2147483630) {
      Result = "ButtonText"}
     else if ($tmp === -2147483629) {
      Result = "InactiveCaptionText"}
     else if ($tmp === -2147483628) {
      Result = "ButtonHighlight"}
     else if ($tmp === -2147483627) {
      Result = "ThreeDDarkShadow"}
     else if ($tmp === -2147483626) {
      Result = "ThreeDHighlight"}
     else if ($tmp === -2147483625) {
      Result = "InfoText"}
     else if ($tmp === -2147483624) {
      Result = "InfoBackground"}
     else {
      R = AColor & 0xFF;
      G = (AColor >>> 8) & 0xFF;
      B = (AColor >>> 16) & 0xFF;
      Result = "#" + pas.SysUtils.IntToHex(R,2) + pas.SysUtils.IntToHex(G,2) + pas.SysUtils.IntToHex(B,2) + pas.SysUtils.IntToHex(A,2);
    };
    return Result;
  };
  this.JSFont = function (AFont) {
    var Result = "";
    Result = "";
    if (AFont != null) {
      if (0 in AFont.FStyle) {
        Result = Result + "bold ";
      };
      if (1 in AFont.FStyle) {
        Result = Result + "italic ";
      };
      Result = Result + pas.SysUtils.IntToStr(AFont.FSize) + "px " + AFont.FName;
    };
    return Result;
  };
  this.JSMeasureText = function (AText, AFontName, AFontSize, AFixedWidth) {
    var Result = pas.Types.TSize.$new();
    var VDiv = null;
    Result.$assign(pas.Types.Size(0,0));
    if (AText !== "") {
      VDiv = document.createElement("div");
      VDiv.style.setProperty("font-family",AFontName);
      VDiv.style.setProperty("font-size",pas.SysUtils.IntToStr(AFontSize) + "px");
      VDiv.style.setProperty("overflow","scroll");
      if (AFixedWidth === 0) {
        VDiv.style.setProperty("display","inline-block");
        VDiv.style.setProperty("white-space","nowrap");
      } else {
        VDiv.style.setProperty("max-width",pas.SysUtils.IntToStr(AFixedWidth) + "px");
        VDiv.style.setProperty("width",pas.SysUtils.IntToStr(AFixedWidth) + "px");
      };
      VDiv.innerHTML = AText;
      document.body.appendChild(VDiv);
      Result.$assign(pas.Types.Size(VDiv.scrollWidth,VDiv.scrollHeight));
      document.body.removeChild(VDiv);
    };
    return Result;
  };
  this.ColorToIdent = function (Color, Ident) {
    var Result = false;
    Result = pas.Classes.IntToIdent(Color,Ident,$impl.Colors);
    return Result;
  };
  this.IdentToColor = function (Ident, Color) {
    var Result = false;
    Result = pas.Classes.IdentToInt(Ident,Color,$impl.Colors);
    return Result;
  };
  $mod.$implcode = function () {
    $impl.Colors$a$clone = function (a) {
      var b = [];
      b.length = 47;
      for (var c = 0; c < 47; c++) b[c] = pas.Classes.TIdentMapEntry.$clone(a[c]);
      return b;
    };
    $impl.Colors = [pas.Classes.TIdentMapEntry.$clone({Value: 0, Name: "clBlack"}),pas.Classes.TIdentMapEntry.$clone({Value: 128, Name: "clMaroon"}),pas.Classes.TIdentMapEntry.$clone({Value: 32768, Name: "clGreen"}),pas.Classes.TIdentMapEntry.$clone({Value: 32896, Name: "clOlive"}),pas.Classes.TIdentMapEntry.$clone({Value: 8388608, Name: "clNavy"}),pas.Classes.TIdentMapEntry.$clone({Value: 8388736, Name: "clPurple"}),pas.Classes.TIdentMapEntry.$clone({Value: 8421376, Name: "clTeal"}),pas.Classes.TIdentMapEntry.$clone({Value: 8421504, Name: "clGray"}),pas.Classes.TIdentMapEntry.$clone({Value: 12632256, Name: "clSilver"}),pas.Classes.TIdentMapEntry.$clone({Value: 255, Name: "clRed"}),pas.Classes.TIdentMapEntry.$clone({Value: 65280, Name: "clLime"}),pas.Classes.TIdentMapEntry.$clone({Value: 65535, Name: "clYellow"}),pas.Classes.TIdentMapEntry.$clone({Value: 16711680, Name: "clBlue"}),pas.Classes.TIdentMapEntry.$clone({Value: 16711935, Name: "clFuchsia"}),pas.Classes.TIdentMapEntry.$clone({Value: 16776960, Name: "clAqua"}),pas.Classes.TIdentMapEntry.$clone({Value: 16777215, Name: "clWhite"}),pas.Classes.TIdentMapEntry.$clone({Value: 12639424, Name: "clMoneyGreen"}),pas.Classes.TIdentMapEntry.$clone({Value: 15780518, Name: "clSkyBlue"}),pas.Classes.TIdentMapEntry.$clone({Value: 15793151, Name: "clCream"}),pas.Classes.TIdentMapEntry.$clone({Value: 10789024, Name: "clMedGray"}),pas.Classes.TIdentMapEntry.$clone({Value: 536870911, Name: "clNone"}),pas.Classes.TIdentMapEntry.$clone({Value: 536870912, Name: "clDefault"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483648, Name: "clScrollBar"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483647, Name: "clBackground"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483646, Name: "clActiveCaption"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483645, Name: "clInactiveCaption"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483644, Name: "clMenu"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483641, Name: "clMenuText"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483643, Name: "clWindow"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483642, Name: "clWindowFrame"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483640, Name: "clWindowText"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483639, Name: "clCaptionText"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483638, Name: "clActiveBorder"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483637, Name: "clInactiveBorder"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483636, Name: "clAppWorkspace"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483635, Name: "clHighlight"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483634, Name: "clHighlightText"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483633, Name: "clBtnFace"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483632, Name: "clBtnShadow"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483631, Name: "clGrayText"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483630, Name: "clBtnText"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483629, Name: "clInactiveCaptionText"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483628, Name: "clBtnHighlight"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483627, Name: "cl3DDkShadow"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483626, Name: "cl3DLight"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483625, Name: "clInfoText"}),pas.Classes.TIdentMapEntry.$clone({Value: -2147483624, Name: "clInfoBk"})];
  };
  $mod.$init = function () {
    pas.Classes.RegisterIntegerConsts(rtl.longint,$mod.IdentToColor,$mod.ColorToIdent);
  };
},[]);
rtl.module("Controls",["System","Classes","SysUtils","Types","JS","Web","Graphics"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.mrNone = 0;
  this.mrOk = 0 + 1;
  this.mrCancel = 0 + 2;
  this.mrAbort = 0 + 3;
  this.mrRetry = 0 + 4;
  this.mrIgnore = 0 + 5;
  this.mrYes = 0 + 6;
  this.mrNo = 0 + 7;
  this.mrAll = 0 + 8;
  this.mrNoToAll = 0 + 9;
  this.mrYesToAll = 0 + 10;
  this.mrClose = 0 + 11;
  this.crDefault = 0;
  this.crNone = -1;
  this.crArrow = -2;
  this.crCross = -3;
  this.crIBeam = -4;
  this.crSize = -22;
  this.crSizeAll = -22;
  this.crSizeNESW = -6;
  this.crSizeNS = -7;
  this.crSizeNWSE = -8;
  this.crSizeWE = -9;
  this.crSizeNW = -23;
  this.crSizeN = -24;
  this.crSizeNE = -25;
  this.crSizeW = -26;
  this.crSizeE = -27;
  this.crSizeSW = -28;
  this.crSizeS = -29;
  this.crSizeSE = -30;
  this.crUpArrow = -10;
  this.crHourGlass = -11;
  this.crDrag = -12;
  this.crNoDrop = -13;
  this.crHSplit = -14;
  this.crVSplit = -15;
  this.crMultiDrag = -16;
  this.crSQLWait = -17;
  this.crNo = -18;
  this.crAppStart = -19;
  this.crHelp = -20;
  this.crHandPoint = -21;
  this.$rtti.$Class("TWinControl");
  this.$rtti.$Class("TControl");
  this.TAlign = {"0": "alNone", alNone: 0, "1": "alTop", alTop: 1, "2": "alBottom", alBottom: 2, "3": "alLeft", alLeft: 3, "4": "alRight", alRight: 4, "5": "alClient", alClient: 5, "6": "alCustom", alCustom: 6};
  this.$rtti.$Enum("TAlign",{minvalue: 0, maxvalue: 6, ordtype: 1, enumtype: this.TAlign});
  this.TAnchorKind = {"0": "akTop", akTop: 0, "1": "akLeft", akLeft: 1, "2": "akRight", akRight: 2, "3": "akBottom", akBottom: 3};
  this.$rtti.$Enum("TAnchorKind",{minvalue: 0, maxvalue: 3, ordtype: 1, enumtype: this.TAnchorKind});
  this.$rtti.$Set("TAnchors",{comptype: this.$rtti["TAnchorKind"]});
  this.TBevelCut = {"0": "bvNone", bvNone: 0, "1": "bvLowered", bvLowered: 1, "2": "bvRaised", bvRaised: 2, "3": "bvSpace", bvSpace: 3};
  this.$rtti.$Enum("TBevelCut",{minvalue: 0, maxvalue: 3, ordtype: 1, enumtype: this.TBevelCut});
  this.TFormBorderStyle = {"0": "bsNone", bsNone: 0, "1": "bsSingle", bsSingle: 1, "2": "bsSizeable", bsSizeable: 2, "3": "bsDialog", bsDialog: 3, "4": "bsToolWindow", bsToolWindow: 4, "5": "bsSizeToolWin", bsSizeToolWin: 5};
  this.$rtti.$Enum("TBorderStyle",{minvalue: 0, maxvalue: 1, ordtype: 1, enumtype: this.TFormBorderStyle});
  this.$rtti.$inherited("TCaption",rtl.string,{});
  this.$rtti.$Int("TCursor",{minvalue: -32768, maxvalue: 32767, ordtype: 2});
  rtl.createClass(this,"TControlCanvas",pas.Graphics.TCanvas,function () {
  });
  this.TShiftStateEnum = {"0": "ssShift", ssShift: 0, "1": "ssAlt", ssAlt: 1, "2": "ssCtrl", ssCtrl: 2, "3": "ssLeft", ssLeft: 3, "4": "ssRight", ssRight: 4, "5": "ssMIDdle", ssMIDdle: 5, "6": "ssDouble", ssDouble: 6};
  this.$rtti.$Enum("TShiftStateEnum",{minvalue: 0, maxvalue: 6, ordtype: 1, enumtype: this.TShiftStateEnum});
  this.$rtti.$Set("TShiftState",{comptype: this.$rtti["TShiftStateEnum"]});
  this.$rtti.$MethodVar("TKeyEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Key",rtl.nativeint,1],["Shift",this.$rtti["TShiftState"]]]), methodkind: 0});
  this.$rtti.$MethodVar("TKeyPressEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Key",rtl.char,1]]), methodkind: 0});
  this.TMouseButton = {"0": "mbLeft", mbLeft: 0, "1": "mbRight", mbRight: 1, "2": "mbMiddle", mbMiddle: 2};
  this.$rtti.$Enum("TMouseButton",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TMouseButton});
  this.$rtti.$MethodVar("TMouseEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Button",this.$rtti["TMouseButton"]],["Shift",this.$rtti["TShiftState"]],["X",rtl.nativeint],["Y",rtl.nativeint]]), methodkind: 0});
  this.$rtti.$MethodVar("TMouseMoveEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Shift",this.$rtti["TShiftState"]],["X",rtl.nativeint],["Y",rtl.nativeint]]), methodkind: 0});
  this.$rtti.$MethodVar("TMouseWheelEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["Shift",this.$rtti["TShiftState"]],["WheelDelta",rtl.nativeint],["MousePos",pas.Types.$rtti["TPoint"]],["Handled",rtl.boolean,1]]), methodkind: 0});
  this.TFocusSearchDirection = {"0": "fsdFirst", fsdFirst: 0, "1": "fsdLast", fsdLast: 1, "2": "fsdNext", fsdNext: 2, "3": "fsdPrev", fsdPrev: 3};
  this.TControlFlag = {"0": "cfInAlignControls", cfInAlignControls: 0};
  rtl.createClass(this,"TControlBorderSpacing",pas.Classes.TPersistent,function () {
    this.$init = function () {
      pas.Classes.TPersistent.$init.call(this);
      this.FAround = 0;
      this.FBottom = 0;
      this.FLeft = 0;
      this.FRight = 0;
      this.FTop = 0;
      this.FUpdateCount = 0;
      this.FOnChange = null;
    };
    this.$final = function () {
      this.FOnChange = undefined;
      pas.Classes.TPersistent.$final.call(this);
    };
    this.SetAround = function (AValue) {
      if (this.FAround !== AValue) {
        this.FAround = AValue;
        this.Changed();
      };
    };
    this.SetBottom = function (AValue) {
      if (this.FBottom !== AValue) {
        this.FBottom = AValue;
        this.Changed();
      };
    };
    this.SetLeft = function (AValue) {
      if (this.FLeft !== AValue) {
        this.FLeft = AValue;
        this.Changed();
      };
    };
    this.SetRight = function (AValue) {
      if (this.FRight !== AValue) {
        this.FRight = AValue;
        this.Changed();
      };
    };
    this.SetTop = function (AValue) {
      if (this.FTop !== AValue) {
        this.FTop = AValue;
        this.Changed();
      };
    };
    this.Changed = function () {
      if ((this.FUpdateCount === 0) && (this.FOnChange != null)) {
        this.FOnChange(this);
      };
    };
    this.Create$1 = function () {
      pas.System.TObject.Create.call(this);
      this.FBottom = 0;
      this.FLeft = 0;
      this.FRight = 0;
      this.FTop = 0;
      this.FUpdateCount = 0;
      return this;
    };
    this.Assign = function (Source) {
      var VSpacing = null;
      if ((Source != null) && $mod.TControlBorderSpacing.isPrototypeOf(Source)) {
        this.BeginUpdate();
        try {
          VSpacing = Source;
          this.FAround = VSpacing.FAround;
          this.FBottom = VSpacing.FBottom;
          this.FLeft = VSpacing.FLeft;
          this.FRight = VSpacing.FRight;
          this.FTop = VSpacing.FTop;
        } finally {
          this.EndUpdate();
        };
      } else {
        pas.Classes.TPersistent.Assign.call(this,Source);
      };
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) {
        this.FUpdateCount -= 1;
        if (this.FUpdateCount === 0) {
          this.Changed();
        };
      };
    };
    var $r = this.$rtti;
    $r.addProperty("Around",2,rtl.nativeint,"FAround","SetAround");
    $r.addProperty("Bottom",2,rtl.nativeint,"FBottom","SetBottom");
    $r.addProperty("Left",2,rtl.nativeint,"FLeft","SetLeft");
    $r.addProperty("Right",2,rtl.nativeint,"FRight","SetRight");
    $r.addProperty("Top",2,rtl.nativeint,"FTop","SetTop");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
  });
  rtl.createClass(this,"TControl",pas.Classes.TComponent,function () {
    this.$init = function () {
      pas.Classes.TComponent.$init.call(this);
      this.FAlign = 0;
      this.FAlpha = 0;
      this.FAnchors = {};
      this.FAutoSize = false;
      this.FBorderSpacing = null;
      this.FBorderStyle = $mod.TFormBorderStyle.bsNone;
      this.FCaption = "";
      this.FColor = 0;
      this.FControlFlags = {};
      this.FControls = null;
      this.FCursor = 0;
      this.FDesignRect = pas.Types.TRect.$new();
      this.FEnabled = false;
      this.FFont = null;
      this.FHandleClass = "";
      this.FHandleElement = null;
      this.FHandleId = "";
      this.FHeight = 0;
      this.FHint = "";
      this.FLeft = 0;
      this.FParent = null;
      this.FParentColor = false;
      this.FParentFont = false;
      this.FParentShowHint = false;
      this.FPopupMenu = null;
      this.FShowHint = false;
      this.FTabOrder = 0;
      this.FTabStop = false;
      this.FTop = 0;
      this.FUpdateCount = 0;
      this.FVisible = false;
      this.FWidth = 0;
      this.FOnClick = null;
      this.FOnDblClick = null;
      this.FOnMouseDown = null;
      this.FOnMouseEnter = null;
      this.FOnMouseLeave = null;
      this.FOnMouseMove = null;
      this.FOnMouseUp = null;
      this.FOnMouseWheel = null;
      this.FOnResize = null;
      this.FOnScroll = null;
    };
    this.$final = function () {
      this.FAnchors = undefined;
      this.FBorderSpacing = undefined;
      this.FControlFlags = undefined;
      this.FControls = undefined;
      this.FDesignRect = undefined;
      this.FFont = undefined;
      this.FHandleElement = undefined;
      this.FParent = undefined;
      this.FPopupMenu = undefined;
      this.FOnClick = undefined;
      this.FOnDblClick = undefined;
      this.FOnMouseDown = undefined;
      this.FOnMouseEnter = undefined;
      this.FOnMouseLeave = undefined;
      this.FOnMouseMove = undefined;
      this.FOnMouseUp = undefined;
      this.FOnMouseWheel = undefined;
      this.FOnResize = undefined;
      this.FOnScroll = undefined;
      pas.Classes.TComponent.$final.call(this);
    };
    this.CheckEventControl = function (AEvent) {
      var Result = false;
      Result = true;
      if (AEvent.target !== this.FHandleElement) if (rtl.isExt(AEvent.target,HTMLDivElement)) {
        Result = false}
       else if (AEvent.target.parentElement !== this.FHandleElement) Result = false;
      return Result;
    };
    this.GetClientHeight = function () {
      var Result = 0;
      Result = this.GetClientRect().Bottom;
      return Result;
    };
    this.GetClientRect = function () {
      var Result = pas.Types.TRect.$new();
      Result.$assign(pas.Types.Rect(0,0,this.FWidth - 1,this.FHeight - 1));
      return Result;
    };
    this.GetClientWidth = function () {
      var Result = 0;
      Result = this.GetClientRect().Right;
      return Result;
    };
    this.GetText = function () {
      var Result = "";
      Result = this.RealGetText();
      return Result;
    };
    this.IsAnchorsStored = function () {
      var Result = false;
      Result = rtl.neSet(this.FAnchors,$mod.AnchorAlign[this.FAlign]);
      return Result;
    };
    this.SetAlign = function (AValue) {
      var oldalign = 0;
      if (this.FAlign !== AValue) {
        oldalign = this.FAlign;
        this.FAlign = AValue;
        if (rtl.eqSet(this.FAnchors,$mod.AnchorAlign[oldalign]) && rtl.neSet(this.FAnchors,$mod.AnchorAlign[this.FAlign])) this.SetAnchors(rtl.refSet($mod.AnchorAlign[this.FAlign]));
        if (this.FParent != null) {
          this.FParent.ReAlign()}
         else this.ReAlign();
      };
    };
    this.SetAlpha = function (AValue) {
      if (this.FAlpha === AValue) return;
      this.FAlpha = AValue;
      this.Changed();
    };
    this.SetAnchors = function (AValue) {
      if (rtl.eqSet(this.FAnchors,AValue)) return;
      this.FAnchors = rtl.refSet(AValue);
    };
    this.SetAutoSize = function (AValue) {
      if (this.FAutoSize !== AValue) {
        this.FAutoSize = AValue;
        if (this.FAutoSize) {
          this.AdjustSize();
        };
      };
    };
    this.SetBorderSpacing = function (AValue) {
      this.FBorderSpacing.Assign(AValue);
    };
    this.SetClientSize = function (AValue) {
      var VClient = pas.Types.TRect.$new();
      VClient.$assign(this.GetClientRect());
      this.SetBounds(this.FLeft,this.FTop,(this.FWidth - VClient.Right) + AValue.x,(this.FHeight - VClient.Bottom) + AValue.y);
    };
    this.SetClientHeight = function (AValue) {
      this.SetClientSize(pas.Types.TPoint.$clone(pas.Types.Point(this.GetClientWidth(),AValue)));
    };
    this.SetClientWidth = function (AValue) {
      this.SetClientSize(pas.Types.TPoint.$clone(pas.Types.Point(AValue,this.GetClientHeight())));
    };
    this.SetColor = function (AValue) {
      if (this.FColor !== AValue) {
        this.FColor = AValue;
        this.FParentColor = false;
        this.ColorChanged(this);
      };
    };
    this.SetCursor = function (AValue) {
      if (this.FCursor !== AValue) {
        this.FCursor = AValue;
        this.Changed();
      };
    };
    this.SetEnabled = function (AValue) {
      if (this.FEnabled !== AValue) {
        this.FEnabled = AValue;
        this.Changed();
      };
    };
    this.SetFont = function (AValue) {
      if (!this.FFont.IsEqual(AValue)) {
        this.FFont.Assign(AValue);
      };
    };
    this.SetHandleClass = function (AValue) {
      if (this.FHandleClass !== AValue) {
        this.FHandleClass = AValue;
        this.Changed();
      };
    };
    this.SetHandleId = function (AValue) {
      if (this.FHandleId !== AValue) {
        this.FHandleId = AValue;
        this.Changed();
      };
    };
    this.SetHeight = function (AValue) {
      this.SetBounds(this.FLeft,this.FTop,this.FWidth,AValue);
    };
    this.SetHint = function (AValue) {
      if (this.FHint !== AValue) {
        this.FHint = AValue;
        this.Changed();
      };
    };
    this.SetLeft = function (AValue) {
      this.SetBounds(AValue,this.FTop,this.FWidth,this.FHeight);
    };
    this.SetParent = function (AValue) {
      if (this.FParent != null) {
        this.FParent.UnRegisterChild(this);
      };
      this.CheckNewParent(AValue);
      this.FParent = AValue;
      if (this.FParent != null) {
        this.FParent.RegisterChild(this);
        this.BeginUpdate();
        try {
          if (this.FParentColor) {
            this.FColor = this.FParent.FColor;
          };
          if (this.FParentFont) {
            this.FFont.Assign(this.FParent.FFont);
          };
          if (this.FParentShowHint) {
            this.FShowHint = this.FParent.FShowHint;
          };
        } finally {
          this.EndUpdate();
        };
      };
    };
    this.SetParentColor = function (AValue) {
      if (this.FParentColor !== AValue) {
        this.FParentColor = AValue;
        if (this.FParentColor && (this.FParent != null)) {
          this.FColor = this.FParent.FColor;
          this.Changed();
        };
      };
    };
    this.SetParentFont = function (AValue) {
      if (this.FParentFont !== AValue) {
        this.FParentFont = AValue;
        if (this.FParentFont && (this.FParent != null) && !this.FFont.IsEqual(this.FParent.FFont)) {
          this.FFont.Assign(this.FParent.FFont);
        };
      };
    };
    this.SetParentShowHint = function (AValue) {
      if (this.FParentShowHint !== AValue) {
        this.FParentShowHint = AValue;
        if (this.FParentShowHint && (this.FParent != null)) {
          this.FShowHint = this.FParent.FShowHint;
          this.Changed();
        };
      };
    };
    this.SetPopupMenu = function (AValue) {
      if (this.FPopupMenu === AValue) return;
      this.FPopupMenu = AValue;
    };
    this.SetShowHint = function (AValue) {
      if (this.FShowHint !== AValue) {
        this.FShowHint = AValue;
        this.FParentShowHint = false;
        this.Changed();
      };
    };
    this.SetTabOrder = function (AValue) {
      if (this.FTabOrder !== AValue) {
        this.FTabOrder = AValue;
        if (this.FParent != null) {
          this.FParent.UpdateTabOrder(this);
        };
      };
    };
    this.SetTabStop = function (AValue) {
      if (this.FTabStop !== AValue) {
        this.FTabStop = AValue;
        this.Changed();
      };
    };
    this.SetText = function (AValue) {
      this.RealSetText(AValue);
    };
    this.SetTop = function (AValue) {
      this.SetBounds(this.FLeft,AValue,this.FWidth,this.FHeight);
    };
    this.SetVisible = function (AValue) {
      if (this.FVisible !== AValue) {
        this.FVisible = AValue;
        this.ReAlign();
      };
    };
    this.SetWidth = function (AValue) {
      this.SetBounds(this.FLeft,this.FTop,AValue,this.FHeight);
    };
    this.SetBorderStyle = function (AValue) {
      if (this.FBorderStyle !== AValue) {
        this.FBorderStyle = AValue;
        this.Changed();
      };
    };
    this.Click = function () {
      if (this.FOnClick != null) {
        this.FOnClick(this);
      };
    };
    this.DblClick = function () {
      if (this.FOnDblClick != null) {
        this.FOnDblClick(this);
      };
    };
    this.DoResize = function () {
      if (this.FOnResize != null) {
        this.FOnResize(this);
      };
    };
    this.DoScroll = function () {
      if (this.FOnScroll != null) {
        this.FOnScroll(this);
      };
    };
    this.MouseDown = function (Button, Shift, X, Y) {
      if (this.FOnMouseDown != null) {
        this.FOnMouseDown(this,Button,rtl.refSet(Shift),X,Y);
      };
    };
    this.MouseEnter = function () {
      if (this.FOnMouseEnter != null) {
        this.FOnMouseEnter(this);
      };
    };
    this.MouseLeave = function () {
      if (this.FOnMouseLeave != null) {
        this.FOnMouseLeave(this);
      };
    };
    this.MouseMove = function (Shift, X, Y) {
      if (this.FOnMouseMove != null) {
        this.FOnMouseMove(this,rtl.refSet(Shift),X,Y);
      };
    };
    this.MouseUp = function (Button, Shift, X, Y) {
      if (this.FOnMouseUp != null) {
        this.FOnMouseUp(this,Button,rtl.refSet(Shift),X,Y);
      };
    };
    this.MouseWeel = function (Shift, WheelDelta, MousePos, Handled) {
      if (this.FOnMouseWheel != null) {
        this.FOnMouseWheel(this,rtl.refSet(Shift),WheelDelta,pas.Types.TPoint.$clone(MousePos),Handled);
      };
    };
    this.HandleClick = function (AEvent) {
      var Result = false;
      if (!this.CheckEventControl(AEvent)) return Result;
      this.Click();
      Result = true;
      return Result;
    };
    this.HandleContextMenu = function (AEvent) {
      var Result = false;
      var X = 0;
      var Y = 0;
      if (!this.CheckEventControl(AEvent)) return Result;
      if (this.FPopupMenu != null) {
        AEvent.preventDefault();
        X = pas.System.Trunc(AEvent.clientX);
        Y = pas.System.Trunc(AEvent.clientY);
        this.FPopupMenu.Popup$1(X,Y);
      };
      Result = true;
      return Result;
    };
    this.HandleDblClick = function (AEvent) {
      var Result = false;
      if (!this.CheckEventControl(AEvent)) return Result;
      this.DblClick();
      Result = true;
      return Result;
    };
    this.HandleMouseDown = function (AEvent) {
      var Result = false;
      var VButton = 0;
      var VOffSets = pas.Types.TRect.$new();
      var VShift = {};
      var X = 0;
      var Y = 0;
      if (!this.CheckEventControl(AEvent)) return Result;
      VButton = $mod.ExtractMouseButton(AEvent);
      VOffSets.$assign($mod.OffSets(this.FHandleElement));
      VShift = rtl.refSet($mod.ExtractShiftState$1(AEvent));
      X = pas.System.Trunc(AEvent.clientX - VOffSets.Left);
      Y = pas.System.Trunc(AEvent.clientY - VOffSets.Top);
      this.MouseDown(VButton,rtl.refSet(VShift),X,Y);
      Result = true;
      return Result;
    };
    this.HandleMouseEnter = function (AEvent) {
      var Result = false;
      if (!this.CheckEventControl(AEvent)) return Result;
      this.MouseEnter();
      Result = true;
      return Result;
    };
    this.HandleMouseLeave = function (AEvent) {
      var Result = false;
      if (!this.CheckEventControl(AEvent)) return Result;
      this.MouseLeave();
      Result = true;
      return Result;
    };
    this.HandleMouseMove = function (AEvent) {
      var Result = false;
      var VOffSets = pas.Types.TRect.$new();
      var VShift = {};
      var X = 0;
      var Y = 0;
      if (!this.CheckEventControl(AEvent)) return Result;
      VOffSets.$assign($mod.OffSets(this.FHandleElement));
      VShift = rtl.refSet($mod.ExtractShiftState$1(AEvent));
      X = pas.System.Trunc(AEvent.clientX - VOffSets.Left);
      Y = pas.System.Trunc(AEvent.clientY - VOffSets.Top);
      this.MouseMove(rtl.refSet(VShift),X,Y);
      Result = true;
      return Result;
    };
    this.HandleMouseUp = function (AEvent) {
      var Result = false;
      var VButton = 0;
      var VOffSets = pas.Types.TRect.$new();
      var VShift = {};
      var X = 0;
      var Y = 0;
      if (!this.CheckEventControl(AEvent)) return Result;
      VButton = $mod.ExtractMouseButton(AEvent);
      VOffSets.$assign($mod.OffSets(this.FHandleElement));
      VShift = rtl.refSet($mod.ExtractShiftState$1(AEvent));
      X = pas.System.Trunc(AEvent.clientX - VOffSets.Left);
      Y = pas.System.Trunc(AEvent.clientY - VOffSets.Top);
      this.MouseUp(VButton,rtl.refSet(VShift),X,Y);
      Result = true;
      return Result;
    };
    this.HandleMouseWheel = function (AEvent) {
      var Result = false;
      var VDelta = 0;
      var VHandled = false;
      var VMousePos = pas.Types.TPoint.$new();
      var VShift = {};
      var VOffSets = pas.Types.TRect.$new();
      if (!this.CheckEventControl(AEvent)) return Result;
      VDelta = pas.System.Trunc(-AEvent.deltaY);
      VHandled = false;
      VOffSets.$assign($mod.OffSets(this.FHandleElement));
      VMousePos.$assign(pas.Types.Point(VOffSets.Left,VOffSets.Top));
      VShift = rtl.refSet($mod.ExtractShiftState$1(AEvent));
      this.MouseWeel(rtl.refSet(VShift),VDelta,pas.Types.TPoint.$clone(VMousePos),{get: function () {
          return VHandled;
        }, set: function (v) {
          VHandled = v;
        }});
      Result = true;
      return Result;
    };
    this.HandleResize = function (AEvent) {
      var Result = false;
      if (!this.CheckEventControl(AEvent)) return Result;
      this.DoResize();
      Result = true;
      return Result;
    };
    this.HandleScroll = function (AEvent) {
      var Result = false;
      if (!this.CheckEventControl(AEvent)) return Result;
      this.DoScroll();
      Result = true;
      return Result;
    };
    this.Loaded = function () {
      pas.Classes.TComponent.Loaded.call(this);
      this.FDesignRect.$assign(pas.Types.Rect(this.FLeft,this.FTop,(this.FLeft + this.FWidth) - 1,(this.FTop + this.FHeight) - 1));
      this.Changed();
    };
    this.Changed = function () {
      var $Self = this;
      var form = null;
      function AdjustWithPPI(aValue) {
        var Result = 0;
        if (form != null) {
          Result = pas.System.Trunc((96 * aValue) / form.FDesignTimePPI)}
         else Result = aValue;
        return Result;
      };
      function FindParentForm() {
        var Result = null;
        var p = null;
        p = $Self.FParent;
        while ((p != null) && !pas.Forms.TCustomForm.isPrototypeOf(p)) p = p.FParent;
        if (pas.Forms.TCustomForm.isPrototypeOf(p)) {
          Result = p}
         else Result = null;
        return Result;
      };
      if (!this.IsUpdating() && rtl.eqSet(rtl.intersectSet(rtl.createSet(0,3),this.FComponentState),{})) {
        form = FindParentForm();
        var $with = this.FHandleElement;
        if (this.FHandleId !== "") {
          $with.setAttribute("id",this.FHandleId);
        } else {
          $with.removeAttribute("id");
        };
        if (this.FHandleClass !== "") {
          $with.setAttribute("class",this.FHandleClass);
        } else {
          $with.removeAttribute("class");
        };
        if ((this.FHandleClass === "") && (this.FHandleId === "")) {
          $with.style.setProperty("color",pas.Graphics.JSColor(this.FFont.FColor,this.FAlpha));
          $mod.UpdateHtmlElementFont(this.FHandleElement,this.FFont,false);
          if (this.FColor in rtl.createSet(536870912,536870911)) {
            $with.style.removeProperty("background-color");
          } else {
            $with.style.setProperty("background-color",pas.Graphics.JSColor(this.FColor,this.FAlpha));
          };
        };
        $with.style.setProperty("left",pas.SysUtils.IntToStr(AdjustWithPPI(this.FLeft)) + "px");
        $with.style.setProperty("top",pas.SysUtils.IntToStr(AdjustWithPPI(this.FTop)) + "px");
        $with.style.setProperty("width",pas.SysUtils.IntToStr(AdjustWithPPI(this.FWidth)) + "px");
        $with.style.setProperty("height",pas.SysUtils.IntToStr(AdjustWithPPI(this.FHeight)) + "px");
        $with.style.setProperty("cursor",$mod.JSCursor(this.FCursor));
        if (this.FEnabled) {
          $with.removeAttribute("disabled");
          $with.style.removeProperty("opacity");
        } else {
          $with.setAttribute("disabled","true");
          $with.style.setProperty("opacity","0.5");
        };
        if (this.FVisible) {
          $with.style.setProperty("visibility","visible");
          $with.style.setProperty("display","block");
        } else {
          $with.style.setProperty("visibility","hidden");
          $with.style.setProperty("display","none");
        };
        if ((this.FHint !== "") && this.FShowHint) {
          $with.setAttribute("title",this.FHint);
        } else {
          $with.removeAttribute("title");
        };
        if (this.FBorderStyle === 0) {
          $with.style.setProperty("border-style","none");
        } else {
          $with.style.removeProperty("border-style");
        };
        $with.setAttribute("tabindex",$mod.IfThen$3(this.FTabStop,"1","-1"));
        $with.style.setProperty("position","absolute");
        $with.style.setProperty("overflow","hidden");
        $with.style.setProperty("-webkit-box-sizing","border-box");
        $with.style.setProperty("-moz-box-sizing","border-box");
        $with.style.setProperty("box-sizing","border-box");
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      throw new Error(pas.SysUtils.Format("%s.CreateHandleElement=nil",pas.System.VarRecs(18,this.$classname)));
      return Result;
    };
    this.RegisterHandleEvents = function () {
      this.FHandleElement.addEventListener("mousedown",rtl.createCallback(this,"HandleMouseDown"));
      var $with = this.FHandleElement;
      $with.addEventListener("click",rtl.createCallback(this,"HandleClick"));
      $with.addEventListener("dblclick",rtl.createCallback(this,"HandleDblClick"));
      $with.addEventListener("mousedown",rtl.createCallback(this,"HandleMouseDown"));
      $with.addEventListener("mouseenter",rtl.createCallback(this,"HandleMouseEnter"));
      $with.addEventListener("mouseleave",rtl.createCallback(this,"HandleMouseLeave"));
      $with.addEventListener("mousemove",rtl.createCallback(this,"HandleMouseMove"));
      $with.addEventListener("mouseup",rtl.createCallback(this,"HandleMouseUp"));
      $with.addEventListener("scroll",rtl.createSafeCallback(this,"HandleScroll"));
      $with.addEventListener("resize",rtl.createSafeCallback(this,"HandleResize"));
      $with.addEventListener("wheel",rtl.createCallback(this,"HandleMouseWheel"));
      $with.addEventListener("contextmenu",rtl.createCallback(this,"HandleContextMenu"));
    };
    this.UnRegisterHandleEvents = function () {
      var $with = this.FHandleElement;
      $with.removeEventListener("click",rtl.createCallback(this,"HandleClick"));
      $with.removeEventListener("dblclick",rtl.createCallback(this,"HandleDblClick"));
      $with.removeEventListener("mousedown",rtl.createCallback(this,"HandleMouseDown"));
      $with.removeEventListener("mouseenter",rtl.createCallback(this,"HandleMouseEnter"));
      $with.removeEventListener("mouseleave",rtl.createCallback(this,"HandleMouseLeave"));
      $with.removeEventListener("mousemove",rtl.createCallback(this,"HandleMouseMove"));
      $with.removeEventListener("mouseup",rtl.createCallback(this,"HandleMouseUp"));
      $with.removeEventListener("scroll",rtl.createSafeCallback(this,"HandleScroll"));
      $with.removeEventListener("resize",rtl.createSafeCallback(this,"HandleResize"));
      $with.removeEventListener("wheel",rtl.createCallback(this,"HandleMouseWheel"));
      $with.removeEventListener("contextmenu",rtl.createCallback(this,"HandleContextMenu"));
    };
    this.CheckNewParent = function (AParent) {
      if ((AParent != null) && !AParent.CheckChildClassAllowed(this.$class.ClassType())) {
        throw new Error(pas.SysUtils.Format("Control of class '%s' can't have control of class '%s' as a child",pas.System.VarRecs(8,AParent.$class.ClassType(),18,this.$classname)));
      };
      if (pas.Forms.TCustomForm.isPrototypeOf(this) && pas.Forms.TCustomForm.isPrototypeOf(AParent)) {
        throw new Error('A "Form" can\'t have another "Form" as parent');
      };
      if (this === AParent) {
        throw new Error('A "Control" can\'t have itself as a Parent');
      };
    };
    this.RegisterChild = function (AControl) {
      var VIndex = 0;
      if (AControl != null) {
        VIndex = this.FControls.indexOf(AControl);
        if (VIndex < 0) {
          this.FControls.push(AControl);
          if (!this.FHandleElement.contains(AControl.FHandleElement)) {
            this.FHandleElement.appendChild(AControl.FHandleElement);
          };
          this.ReAlign();
          AControl.SetTabOrder(this.FControls.length);
        };
      };
    };
    this.UnRegisterChild = function (AControl) {
      var VIndex = 0;
      if (AControl != null) {
        VIndex = this.FControls.indexOf(AControl);
        if (VIndex >= 0) {
          this.FControls.splice(VIndex,1);
          if (this.FHandleElement.contains(AControl.FHandleElement)) {
            this.FHandleElement.removeChild(AControl.FHandleElement);
          };
          this.ReAlign();
          this.UpdateTabOrder(null);
        };
      };
    };
    this.AlignControls = function () {
      var $Self = this;
      var VControl = null;
      var VSpacing = null;
      var VIndex = 0;
      var VLeft = 0;
      var VTop = 0;
      var VRight = 0;
      var VBotton = 0;
      var VWidth = 0;
      var newleft = 0;
      var newtop = 0;
      var newright = 0;
      var newbottom = 0;
      if (0 in this.FControlFlags) return;
      this.FControlFlags = rtl.includeSet(this.FControlFlags,0);
      this.BeginUpdate();
      try {
        VLeft = 0;
        VTop = 0;
        VRight = this.FWidth;
        VBotton = this.FHeight;
        VWidth = this.FWidth;
        for (var $l = 0, $end = this.FControls.length - 1; $l <= $end; $l++) {
          VIndex = $l;
          VControl = rtl.getObject(this.FControls[VIndex]);
          if ((VControl != null) && (VControl.FAlign === 1) && VControl.FVisible) {
            VControl.BeginUpdate();
            try {
              VSpacing = VControl.FBorderSpacing;
              VControl.SetLeft(VLeft + VSpacing.FLeft + VSpacing.FAround);
              VControl.SetTop(VTop + VSpacing.FTop + VSpacing.FAround);
              VControl.SetWidth(VWidth - VSpacing.FLeft - VSpacing.FRight - (VSpacing.FAround * 2));
              VControl.SetHeight(VControl.FHeight);
            } finally {
              VControl.EndUpdate();
            };
            VTop = VTop + VControl.FHeight + VSpacing.FTop + VSpacing.FBottom + (VSpacing.FAround * 2);
          };
        };
        if (VTop < 0) {
          VTop = 0;
        };
        for (var $l1 = 0, $end1 = this.FControls.length - 1; $l1 <= $end1; $l1++) {
          VIndex = $l1;
          VControl = rtl.getObject(this.FControls[VIndex]);
          if ((VControl != null) && (VControl.FAlign === 2) && VControl.FVisible) {
            VControl.BeginUpdate();
            try {
              VSpacing = VControl.FBorderSpacing;
              VControl.SetLeft(VLeft + VSpacing.FLeft + VSpacing.FAround);
              if (!(0 in VControl.FAnchors)) {
                VControl.SetTop(VBotton - VControl.FHeight - VSpacing.FBottom - VSpacing.FAround)}
               else VControl.SetTop(VControl.FTop);
              VControl.SetWidth(VWidth - VSpacing.FLeft - VSpacing.FRight - (VSpacing.FAround * 2));
              if (!(0 in VControl.FAnchors)) {
                VControl.SetHeight(VControl.FHeight)}
               else VControl.SetHeight(VBotton - VControl.FTop);
            } finally {
              VControl.EndUpdate();
            };
            VBotton = VBotton - VControl.FHeight - VSpacing.FTop - VSpacing.FBottom - (VSpacing.FAround * 2);
          };
        };
        if (VBotton < 0) {
          VBotton = 0;
        };
        for (var $l2 = 0, $end2 = this.FControls.length - 1; $l2 <= $end2; $l2++) {
          VIndex = $l2;
          VControl = rtl.getObject(this.FControls[VIndex]);
          if ((VControl != null) && (VControl.FAlign === 3) && VControl.FVisible) {
            VControl.BeginUpdate();
            try {
              VSpacing = VControl.FBorderSpacing;
              VControl.SetLeft(VLeft + VSpacing.FLeft + VSpacing.FAround);
              VControl.SetTop(VTop + VSpacing.FTop + VSpacing.FAround);
              VControl.SetWidth(VControl.FWidth);
              VControl.SetHeight(VBotton - VTop - VSpacing.FTop - VSpacing.FBottom - (VSpacing.FAround * 2));
            } finally {
              VControl.EndUpdate();
            };
            VLeft = VLeft + VControl.FWidth + VSpacing.FLeft + VSpacing.FRight + (VSpacing.FAround * 2);
          };
        };
        if (VLeft < 0) {
          VLeft = 0;
        };
        for (var $l3 = 0, $end3 = this.FControls.length - 1; $l3 <= $end3; $l3++) {
          VIndex = $l3;
          VControl = rtl.getObject(this.FControls[VIndex]);
          if ((VControl != null) && (VControl.FAlign === 4) && VControl.FVisible) {
            VControl.BeginUpdate();
            try {
              VSpacing = VControl.FBorderSpacing;
              if (!(1 in VControl.FAnchors)) {
                VControl.SetLeft(VRight - VControl.FWidth - VSpacing.FRight - VSpacing.FAround)}
               else VControl.SetLeft(VControl.FLeft);
              VControl.SetTop(VTop + VSpacing.FTop + VSpacing.FAround);
              if (!(1 in VControl.FAnchors)) {
                VControl.SetWidth(VControl.FWidth)}
               else VControl.SetWidth(VRight - VControl.FLeft);
              VControl.SetHeight(VBotton - VTop - VSpacing.FTop - VSpacing.FBottom - (VSpacing.FAround * 2));
            } finally {
              VControl.EndUpdate();
            };
            VRight = VRight - VControl.FWidth - VSpacing.FLeft - VSpacing.FRight - (VSpacing.FAround * 2);
          };
        };
        if (VRight < 0) {
          VRight = 0;
        };
        for (var $l4 = 0, $end4 = this.FControls.length - 1; $l4 <= $end4; $l4++) {
          VIndex = $l4;
          VControl = rtl.getObject(this.FControls[VIndex]);
          if ((VControl != null) && (VControl.FAlign === 5) && VControl.FVisible) {
            VControl.BeginUpdate();
            try {
              VSpacing = VControl.FBorderSpacing;
              VControl.SetLeft(VLeft + VSpacing.FLeft + VSpacing.FAround);
              VControl.SetTop(VTop + VSpacing.FTop + VSpacing.FAround);
              VControl.SetWidth(VRight - VLeft - VSpacing.FLeft - VSpacing.FRight - (VSpacing.FAround * 2));
              VControl.SetHeight(VBotton - VTop - VSpacing.FTop - VSpacing.FBottom - (VSpacing.FAround * 2));
            } finally {
              VControl.EndUpdate();
            };
          };
        };
        for (var $l5 = 0, $end5 = this.FControls.length - 1; $l5 <= $end5; $l5++) {
          VIndex = $l5;
          VControl = rtl.getObject(this.FControls[VIndex]);
          if ((VControl != null) && (VControl.FAlign === 0) && VControl.FVisible && rtl.neSet(VControl.FAnchors,{})) {
            VControl.BeginUpdate();
            try {
              if (1 in VControl.FAnchors) newleft = VControl.FLeft;
              if (0 in VControl.FAnchors) newtop = VControl.FTop;
              if (3 in VControl.FAnchors) newbottom = this.FHeight - (this.FDesignRect.Bottom - VControl.FDesignRect.Bottom);
              if (2 in VControl.FAnchors) newright = this.FWidth - (this.FDesignRect.Right - VControl.FDesignRect.Right);
              if (rtl.leSet(rtl.createSet(1,2),VControl.FAnchors)) {
                VControl.SetLeft(newleft);
                VControl.SetWidth((newright - newleft) + 1);
              } else if (1 in VControl.FAnchors) {
                VControl.SetLeft(newleft)}
               else if (2 in VControl.FAnchors) VControl.SetLeft(newright - VControl.FWidth);
              if (rtl.leSet(rtl.createSet(0,3),VControl.FAnchors)) {
                VControl.SetTop(newtop);
                VControl.SetHeight((newbottom - newtop) + 1);
              } else if (0 in VControl.FAnchors) {
                VControl.SetTop(newtop)}
               else if (3 in VControl.FAnchors) VControl.SetTop(newbottom - VControl.FHeight);
            } finally {
              VControl.EndUpdate();
            };
          };
        };
      } finally {
        this.FControlFlags = rtl.excludeSet(this.FControlFlags,0);
        this.EndUpdate();
      };
    };
    this.RealGetText = function () {
      var Result = "";
      Result = this.FCaption;
      return Result;
    };
    this.RealSetText = function (AValue) {
      if (this.FCaption !== AValue) {
        this.FCaption = AValue;
        this.Changed();
      };
    };
    this.BorderSpacingChanged = function (Sender) {
      if (this.FParent != null) {
        this.FParent.AlignControls();
      };
    };
    this.ColorChanged = function (Sender) {
      this.Changed();
    };
    this.FontChanged = function (Sender) {
      this.Changed();
    };
    this.TabOrderArray = function () {
      var Result = null;
      Result = this.FControls.slice(0).sort(rtl.createCallback(this,"CompareTabOrder"));
      return Result;
    };
    this.CompareTabOrder = function (A, B) {
      var Result = 0;
      if (pas.System.Assigned(A) && pas.System.Assigned(B) && rtl.isExt(A,$mod.TControl,1) && rtl.isExt(B,$mod.TControl,1)) {
        Result = rtl.getObject(A).FTabOrder - rtl.getObject(B).FTabOrder;
      } else {
        Result = 0;
      };
      return Result;
    };
    this.UpdateTabOrder = function (AValue) {
      var VControl = null;
      var VArray = null;
      var VIndex = 0;
      if (AValue != null) {
        for (var $l = 0, $end = this.FControls.length - 1; $l <= $end; $l++) {
          VIndex = $l;
          VControl = rtl.getObject(this.FControls[VIndex]);
          if ((VControl != null) && (VControl !== AValue) && (VControl.FTabOrder >= AValue.FTabOrder)) {
            VControl.FTabOrder += 1;
          };
        };
      };
      VArray = this.TabOrderArray();
      try {
        for (var $l1 = 0, $end1 = VArray.length - 1; $l1 <= $end1; $l1++) {
          VIndex = $l1;
          VControl = rtl.getObject(VArray[VIndex]);
          if (VControl != null) {
            VControl.BeginUpdate();
            try {
              VControl.FTabOrder = VIndex;
            } finally {
              VControl.EndUpdate();
            };
          };
        };
      } finally {
        VArray.length = 0;
      };
    };
    this.SetParentComponent = function (AValue) {
      if ($mod.TWinControl.isPrototypeOf(AValue)) this.SetParent(AValue);
    };
    this.GetControlClassDefaultSize = function () {
      var Result = pas.Types.TSize.$new();
      Result.cx = 75;
      Result.cy = 50;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      var sz = pas.Types.TSize.$new();
      pas.Classes.TComponent.Create$1.call(this,AOwner);
      this.FHandleElement = this.CreateHandleElement();
      this.FHandleClass = "";
      this.FHandleId = "";
      this.RegisterHandleEvents();
      this.FControls = new Array();
      this.FBorderSpacing = $mod.TControlBorderSpacing.$create("Create$1");
      this.FBorderSpacing.FOnChange = rtl.createCallback(this,"BorderSpacingChanged");
      this.FBorderStyle = 1;
      this.FFont = pas.Graphics.TFont.$create("Create$1");
      this.FFont.FOnChange = rtl.createCallback(this,"FontChanged");
      this.FAlign = 0;
      this.FAlpha = 255;
      this.FAnchors = rtl.createSet(1,0);
      this.FAutoSize = false;
      this.FCaption = "";
      this.FColor = 536870912;
      this.FCursor = 0;
      sz.$assign(this.$class.GetControlClassDefaultSize());
      this.FDesignRect.$assign(pas.Types.Rect(0,0,sz.cx - 1,sz.cy - 1));
      this.FEnabled = true;
      this.FLeft = 0;
      this.FParent = null;
      this.FParentColor = false;
      this.FParentFont = true;
      this.FParentShowHint = true;
      this.FShowHint = false;
      this.FTabOrder = 0;
      this.FTabStop = true;
      this.FTop = 0;
      this.FUpdateCount = 0;
      this.FVisible = true;
      return this;
    };
    this.Destroy = function () {
      if (this.FHandleElement != null) this.UnRegisterHandleEvents();
      if (this.FParent != null) {
        this.FParent.UnRegisterChild(this);
      };
      if (this.FControls != null) this.FControls.length = 0;
      rtl.free(this,"FBorderSpacing");
      this.FBorderSpacing = null;
      rtl.free(this,"FFont");
      this.FFont = null;
      pas.Classes.TComponent.Destroy.call(this);
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) {
        this.FUpdateCount -= 1;
        if (this.FUpdateCount === 0) {
          this.Changed();
        };
      };
    };
    this.AdjustSize = function () {
    };
    this.IsUpdating = function () {
      var Result = false;
      Result = this.FUpdateCount > 0;
      return Result;
    };
    this.Invalidate = function () {
    };
    this.ReAlign = function () {
      this.AlignControls();
      if (this.FParent != null) {
        this.FParent.ReAlign();
      };
      this.Invalidate();
    };
    this.BringToFront = function () {
      var VParentElement = null;
      VParentElement = this.FHandleElement.parentElement;
      if (VParentElement != null) {
        VParentElement.removeChild(this.FHandleElement);
        VParentElement.appendChild(this.FHandleElement);
      };
    };
    this.SetBounds = function (ALeft, ATop, AWidth, AHeight) {
      if ((this.FLeft !== ALeft) || (this.FTop !== ATop) || (this.FWidth !== AWidth) || (this.FHeight !== AHeight)) {
        this.FLeft = ALeft;
        this.FTop = ATop;
        if (AWidth > 0) {
          this.FWidth = AWidth;
        } else {
          this.FWidth = 0;
        };
        if (AHeight > 0) {
          this.FHeight = AHeight;
        } else {
          this.FHeight = 0;
        };
        this.Changed();
        this.ReAlign();
      };
    };
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Cursor",2,$mod.$rtti["TCursor"],"FCursor","SetCursor");
    $r.addProperty("Left",2,rtl.nativeint,"FLeft","SetLeft");
    $r.addProperty("Height",2,rtl.nativeint,"FHeight","SetHeight");
    $r.addProperty("Hint",2,rtl.string,"FHint","SetHint");
    $r.addProperty("Top",2,rtl.nativeint,"FTop","SetTop");
    $r.addProperty("Width",2,rtl.nativeint,"FWidth","SetWidth");
  });
  rtl.createClass(this,"TWinControl",this.TControl,function () {
    this.$init = function () {
      $mod.TControl.$init.call(this);
      this.FOnEnter = null;
      this.FOnExit = null;
      this.FOnKeyDown = null;
      this.FOnKeyPress = null;
      this.FOnKeyUp = null;
    };
    this.$final = function () {
      this.FOnEnter = undefined;
      this.FOnExit = undefined;
      this.FOnKeyDown = undefined;
      this.FOnKeyPress = undefined;
      this.FOnKeyUp = undefined;
      $mod.TControl.$final.call(this);
    };
    this.GetControl = function (AIndex) {
      var Result = null;
      Result = rtl.getObject(this.FControls[AIndex]);
      return Result;
    };
    this.GetControlCount = function () {
      var Result = 0;
      Result = this.FControls.length;
      return Result;
    };
    this.DoEnter = function () {
      if (this.FOnEnter != null) {
        this.FOnEnter(this);
      };
    };
    this.DoExit = function () {
      if (this.FOnExit != null) {
        this.FOnExit(this);
      };
    };
    this.KeyDown = function (Key, Shift) {
      if (this.FOnKeyDown != null) {
        this.FOnKeyDown(this,Key,rtl.refSet(Shift));
      };
    };
    this.KeyPress = function (Key) {
      if (this.FOnKeyPress != null) {
        this.FOnKeyPress(this,Key);
      };
    };
    this.KeyUp = function (Key, Shift) {
      if (this.FOnKeyUp != null) {
        this.FOnKeyUp(this,Key,rtl.refSet(Shift));
      };
    };
    this.HandleEnter = function (AEvent) {
      var Result = false;
      var VParent = null;
      VParent = this.FParent;
      while (VParent != null) {
        if (pas.Forms.TCustomForm.isPrototypeOf(VParent)) {
          VParent.SetActiveControl(this);
          break;
        };
        VParent = VParent.FParent;
      };
      AEvent.stopPropagation();
      this.DoEnter();
      Result = true;
      return Result;
    };
    this.HandleExit = function (AEvent) {
      var Result = false;
      AEvent.stopPropagation();
      this.DoExit();
      Result = true;
      return Result;
    };
    this.HandleKeyDown = function (AEvent) {
      var Result = false;
      var VControl = null;
      var VForm = null;
      var VKey = 0;
      var VParent = null;
      var VShift = {};
      VParent = this.FParent;
      while (VParent != null) {
        if (pas.Forms.TCustomForm.isPrototypeOf(VParent)) {
          VForm = VParent;
          if (VForm.FKeyPreview && VForm.HandleKeyDown(AEvent)) {
            Result = true;
            return Result;
          };
        };
        VParent = VParent.FParent;
      };
      VKey = $mod.ExtractKeyCode(AEvent);
      VShift = rtl.refSet($mod.ExtractShiftState(AEvent));
      AEvent.stopPropagation();
      this.KeyDown({get: function () {
          return VKey;
        }, set: function (v) {
          VKey = v;
        }},rtl.refSet(VShift));
      if (VKey === 0) {
        AEvent.preventDefault();
      } else {
        var $tmp = VKey;
        if ($tmp === 9) {
          if (this.FParent != null) {
            if (0 in VShift) {
              VControl = this.FParent.FindFocusControl(this,3);
              if (!(VControl != null)) {
                VControl = this.FParent.FindFocusControl(null,1);
              };
            } else {
              VControl = this.FParent.FindFocusControl(this,2);
              if (!(VControl != null)) {
                VControl = this.FParent.FindFocusControl(null,0);
              };
            };
            if ((VControl != null) && VControl.CanSetFocus()) {
              VControl.SetFocus();
            };
            AEvent.preventDefault();
          };
        };
      };
      Result = true;
      return Result;
    };
    this.HandleKeyUp = function (AEvent) {
      var Result = false;
      var VForm = null;
      var VKey = 0;
      var VParent = null;
      var VShift = {};
      VParent = this.FParent;
      while (VParent != null) {
        if (pas.Forms.TCustomForm.isPrototypeOf(VParent)) {
          VForm = VParent;
          if (VForm.FKeyPreview && VForm.HandleKeyUp(AEvent)) {
            Result = true;
            return Result;
          };
        };
        VParent = VParent.FParent;
      };
      VKey = $mod.ExtractKeyCode(AEvent);
      VShift = rtl.refSet($mod.ExtractShiftState(AEvent));
      AEvent.stopPropagation();
      this.KeyUp({get: function () {
          return VKey;
        }, set: function (v) {
          VKey = v;
        }},rtl.refSet(VShift));
      if (VKey === 0) {
        AEvent.preventDefault();
      };
      Result = true;
      return Result;
    };
    this.HandleKeyPress = function (AEvent) {
      var Result = false;
      var VForm = null;
      var VKey = "";
      var VParent = null;
      VParent = this.FParent;
      while (VParent != null) {
        if (pas.Forms.TCustomForm.isPrototypeOf(VParent)) {
          VForm = VParent;
          if (VForm.FKeyPreview && VForm.HandleKeyPress(AEvent)) {
            Result = true;
            return Result;
          };
        };
        VParent = VParent.FParent;
      };
      AEvent.stopPropagation();
      VKey = $mod.ExtractKeyChar(AEvent);
      if (VKey === "\x00") {
        AEvent.preventDefault();
      } else {
        this.KeyPress({get: function () {
            return VKey;
          }, set: function (v) {
            VKey = v;
          }});
        if (VKey === "\x00") {
          AEvent.preventDefault();
        };
      };
      Result = true;
      return Result;
    };
    this.RegisterHandleEvents = function () {
      $mod.TControl.RegisterHandleEvents.call(this);
      var $with = this.FHandleElement;
      $with.addEventListener("focus",rtl.createSafeCallback(this,"HandleEnter"));
      $with.addEventListener("blur",rtl.createSafeCallback(this,"HandleExit"));
      $with.addEventListener("keydown",rtl.createCallback(this,"HandleKeyDown"));
      $with.addEventListener("keypress",rtl.createCallback(this,"HandleKeyPress"));
      $with.addEventListener("keyup",rtl.createCallback(this,"HandleKeyUp"));
    };
    this.UnRegisterHandleEvents = function () {
      $mod.TControl.UnRegisterHandleEvents.call(this);
      var $with = this.FHandleElement;
      $with.removeEventListener("focus",rtl.createSafeCallback(this,"HandleEnter"));
      $with.removeEventListener("blur",rtl.createSafeCallback(this,"HandleExit"));
      $with.removeEventListener("keydown",rtl.createCallback(this,"HandleKeyDown"));
      $with.removeEventListener("keypress",rtl.createCallback(this,"HandleKeyPress"));
      $with.removeEventListener("keyup",rtl.createCallback(this,"HandleKeyUp"));
    };
    this.CheckChildClassAllowed = function (AChildClass) {
      var Result = false;
      Result = (AChildClass != null) && AChildClass.InheritsFrom($mod.TControl);
      return Result;
    };
    this.FindFocusControl = function (AStartControl, ADirection) {
      var Result = null;
      var VControl = null;
      var VArray = null;
      var VIndex = 0;
      var VTabOrder = 0;
      Result = null;
      VArray = this.TabOrderArray();
      if (VArray.length === 0) {
        return Result;
      };
      try {
        VTabOrder = VArray.indexOf(AStartControl);
        if (VTabOrder < 0) {
          if (ADirection in rtl.createSet(0)) {
            VTabOrder = VArray.length - 1;
          } else {
            VTabOrder = 0;
          };
        };
        var $tmp = ADirection;
        if ($tmp === 0) {
          VControl = rtl.getObject(VArray[0]);
          if ((VControl != null) && $mod.TWinControl.isPrototypeOf(VControl) && VControl.FEnabled && VControl.FVisible && VControl.FTabStop) {
            return VControl;
          };
        } else if ($tmp === 1) {
          VControl = rtl.getObject(VArray[VArray.length - 1]);
          if ((VControl != null) && $mod.TWinControl.isPrototypeOf(VControl) && VControl.FEnabled && VControl.FVisible && VControl.FTabStop) {
            return VControl;
          };
        } else if ($tmp === 2) {
          if (VTabOrder < (VArray.length - 1)) {
            for (var $l = VTabOrder + 1, $end = VArray.length - 1; $l <= $end; $l++) {
              VIndex = $l;
              VControl = rtl.getObject(VArray[VIndex]);
              if ((VControl != null) && $mod.TWinControl.isPrototypeOf(VControl) && VControl.FEnabled && VControl.FVisible && VControl.FTabStop) {
                return VControl;
              };
            };
          };
        } else if ($tmp === 3) {
          if (VTabOrder > 0) {
            for (var $l1 = VTabOrder - 1; $l1 >= 0; $l1--) {
              VIndex = $l1;
              VControl = rtl.getObject(VArray[VIndex]);
              if ((VControl != null) && $mod.TWinControl.isPrototypeOf(VControl) && VControl.FEnabled && VControl.FVisible && VControl.FTabStop) {
                return VControl;
              };
            };
          };
        };
      } finally {
        VArray.length = 0;
      };
      return Result;
    };
    this.CanSetFocus = function () {
      var Result = false;
      var VControl = null;
      VControl = this;
      while (true) {
        if (!VControl.FVisible && VControl.FEnabled) {
          Result = false;
          return Result;
        };
        if (VControl.FParent != null) {
          VControl = VControl.FParent;
        } else {
          break;
        };
      };
      Result = (VControl != null) && pas.Forms.TCustomForm.isPrototypeOf(VControl);
      return Result;
    };
    this.SetFocus = function () {
      this.FHandleElement.focus();
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  rtl.createClass(this,"TCustomPopupMenu",this.TWinControl,function () {
    this.$init = function () {
      $mod.TWinControl.$init.call(this);
      this.FOwner$1 = null;
    };
    this.$final = function () {
      this.FOwner$1 = undefined;
      $mod.TWinControl.$final.call(this);
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("div");
      Result.style.setProperty("display","none");
      Result.style.setProperty("visibility","hidden");
      this.SetVisible(false);
      return Result;
    };
    this.Changed = function () {
      $mod.TControl.Changed.call(this);
      this.FHandleElement.style.setProperty("overflow","");
      this.FHandleElement.style.setProperty("position","fixed");
    };
    this.Create$1 = function (AOwner) {
      $mod.TControl.Create$1.call(this,AOwner);
      this.FOwner$1 = AOwner;
      return this;
    };
    this.Destroy = function () {
      $mod.TControl.Destroy.call(this);
    };
    this.Popup$1 = function (Ax, Ay) {
      var i = 0;
      if ($mod.TWinControl.isPrototypeOf(this.FOwner$1)) for (var $l = 0, $end = this.FOwner$1.GetControlCount() - 1; $l <= $end; $l++) {
        i = $l;
        if ($mod.TCustomPopupMenu.isPrototypeOf(this.FOwner$1.GetControl(i))) this.FOwner$1.GetControl(i).SetVisible(false);
      };
      this.FHandleElement.style.setProperty("background-color","#f1f1f1");
      this.FHandleElement.style.setProperty("box-shadow","0px 8px 16px 0px rgba(0,0,0,0.2)");
      this.FHandleElement.style.setProperty("overflow","auto");
      this.FHandleElement.style.setProperty("z-index","1");
      for (var $l1 = 0, $end1 = this.GetControlCount() - 1; $l1 <= $end1; $l1++) {
        i = $l1;
        this.GetControl(i).SetBounds(this.FParent.FLeft,27 * i,200,27);
      };
      this.SetBounds(Ax,Ay,200,this.GetControlCount() * 27);
      this.SetVisible(true);
    };
    this.Hide = function () {
      this.SetVisible(false);
    };
    this.Loaded = function () {
      var $Self = this;
      var i = 0;
      function MoveElement(AControl) {
        var j = 0;
        if ($mod.TCustomMenuItem.isPrototypeOf(AControl)) {
          for (var $l = 0, $end = AControl.GetControlCount() - 1; $l <= $end; $l++) {
            j = $l;
            AControl.FSubMenu.append(AControl.GetControl(j).FHandleElement);
            AControl.GetControl(j).FHandleElement.style.setProperty("position","");
            AControl.GetControl(j).FHandleElement.style.setProperty("left","0px");
            if (AControl.GetControl(j).GetControlCount() > 0) MoveElement(AControl.GetControl(j));
          };
          AControl.FSubMenu.style.setProperty("height",pas.SysUtils.IntToStr(AControl.GetControlCount() * 27) + "px");
        };
      };
      $mod.TControl.Loaded.call(this);
      this.Hide();
      for (var $l = 0, $end = this.GetControlCount() - 1; $l <= $end; $l++) {
        i = $l;
        MoveElement(this.GetControl(i));
      };
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  rtl.createClass(this,"TCustomMenuItem",this.TWinControl,function () {
    this.$init = function () {
      $mod.TWinControl.$init.call(this);
      this.FTextElement = null;
      this.FSubMenu = null;
    };
    this.$final = function () {
      this.FTextElement = undefined;
      this.FSubMenu = undefined;
      $mod.TWinControl.$final.call(this);
    };
    this.TextClickHandler = function (aEvent) {
      var Result = false;
      this.Click();
      return Result;
    };
    this.OnMenuMouseEnter = function (Sender) {
      var i = 0;
      this.FOnMouseEnter;
      for (var $l = 0, $end = this.GetControlCount() - 1; $l <= $end; $l++) {
        i = $l;
        this.GetControl(i).SetBounds(0,27 * i,200,27);
      };
      this.FSubMenu.style.setProperty("display","block");
      this.FSubMenu.style.setProperty("visibility","visible");
      this.FHandleElement.style.setProperty("background-color","#ddd");
    };
    this.OnMenuMouseLeave = function (Sender) {
      this.FOnMouseLeave;
      this.FHandleElement.style.setProperty("background-color","#f1f1f1");
      this.FSubMenu.style.setProperty("display","none");
      this.FSubMenu.style.setProperty("visibility","hidden");
    };
    this.Click = function () {
      var obj = null;
      $mod.TControl.Click.call(this);
      obj = this.FParent;
      while ((obj != null) && !$mod.TCustomPopupMenu.isPrototypeOf(obj)) obj = obj.FParent;
      if (obj != null) obj.Hide();
    };
    this.Changed = function () {
      $mod.TControl.Changed.call(this);
      this.FHandleElement.style.setProperty("padding","6px 16px");
      this.FHandleElement.style.setProperty("background-color","#f1f1f1");
      this.FHandleElement.style.setProperty("overflow","");
      this.FTextElement.innerText = this.GetText();
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("div");
      Result.style.setProperty("visibility","hidden");
      Result.style.setProperty("display","none");
      this.FTextElement = document.createElement("div");
      this.FTextElement.style.setProperty("cursor","default");
      Result.appendChild(this.FTextElement);
      this.FSubMenu = document.createElement("div");
      this.FSubMenu.style.setProperty("visibility","hidden");
      this.FSubMenu.style.setProperty("display","none");
      this.FSubMenu.style.setProperty("width","200px");
      this.FSubMenu.style.setProperty("left","200px");
      this.FSubMenu.style.setProperty("top","0px");
      this.FSubMenu.style.setProperty("position","absolute");
      this.FSubMenu.style.setProperty("box-shadow","0px 8px 16px 0px rgba(0,0,0,0.2)");
      this.FSubMenu.style.setProperty("box-sizing","border-box");
      this.FSubMenu.style.setProperty("z-index","2");
      this.FSubMenu.className = "QQQ";
      Result.appendChild(this.FSubMenu);
      this.SetVisible(false);
      return Result;
    };
    this.Create$1 = function (AOwner) {
      $mod.TControl.Create$1.call(this,AOwner);
      this.SetBounds(0,0,200,27);
      this.FOnMouseEnter = rtl.createCallback(this,"OnMenuMouseEnter");
      this.FOnMouseLeave = rtl.createCallback(this,"OnMenuMouseLeave");
      this.FTextElement.onclick = rtl.createSafeCallback(this,"TextClickHandler");
      this.SetVisible(true);
      return this;
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  rtl.createClass(this,"TCustomControl",this.TWinControl,function () {
    this.$init = function () {
      $mod.TWinControl.$init.call(this);
      this.FCanvas = null;
      this.FOnPaint = null;
    };
    this.$final = function () {
      this.FCanvas = undefined;
      this.FOnPaint = undefined;
      $mod.TWinControl.$final.call(this);
    };
    this.ColorChanged = function (Sender) {
      if (this.FCanvas != null) {
        this.FCanvas.FBrush.SetColor(this.FColor);
      };
      $mod.TControl.ColorChanged.call(this,Sender);
    };
    this.FontChanged = function (Sender) {
      if (this.FCanvas != null) {
        this.FCanvas.FFont.Assign(this.FFont);
      };
      $mod.TControl.FontChanged.call(this,Sender);
    };
    this.Paint = function () {
      if (this.FOnPaint != null) {
        this.FOnPaint(this);
      };
    };
    this.Destroy = function () {
      if (this.FCanvas != null) {
        this.FCanvas.$destroy("Destroy");
        this.FCanvas = null;
      };
      $mod.TControl.Destroy.call(this);
    };
    this.Invalidate = function () {
      $mod.TControl.Invalidate.call(this);
      this.Paint();
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  this.AnchorAlign$a$clone = function (a) {
    var b = [];
    b.length = 7;
    for (var c = 0; c < 7; c++) b[c] = rtl.refSet(a[c]);
    return b;
  };
  this.AnchorAlign = [rtl.createSet(1,0),rtl.createSet(1,0,2),rtl.createSet(1,2,3),rtl.createSet(1,0,3),rtl.createSet(2,0,3),rtl.createSet(1,0,2,3),rtl.createSet(1,0)];
  this.IfThen$2 = function (AExpression, AConsequence, AAlternative) {
    var Result = 0;
    if (AExpression) {
      Result = AConsequence;
    } else {
      Result = AAlternative;
    };
    return Result;
  };
  this.IfThen$3 = function (AExpression, AConsequence, AAlternative) {
    var Result = "";
    if (AExpression) {
      Result = AConsequence;
    } else {
      Result = AAlternative;
    };
    return Result;
  };
  this.OffSets = function (AElement) {
    var Result = pas.Types.TRect.$new();
    Result.$assign(pas.Types.Rect(0,0,0,0));
    if (AElement != null) {
      var $with = AElement.getBoundingClientRect();
      Result.Left = pas.System.Trunc($with.left + window.scrollX);
      Result.Top = pas.System.Trunc($with.top + window.scrollY);
    };
    return Result;
  };
  this.UpdateHtmlElementFont = function (AElement, AFont, AClear) {
    var s = "";
    var $with = AElement.style;
    if (AClear) {
      $with.removeProperty("font-family");
      $with.removeProperty("font-size");
      $with.removeProperty("font-weight");
      $with.removeProperty("font-style");
      $with.removeProperty("text-decoration");
    } else {
      $with.setProperty("font-family",AFont.FName);
      $with.setProperty("font-size",pas.SysUtils.IntToStr(AFont.FSize) + "pt");
      if (0 in AFont.FStyle) {
        $with.setProperty("font-weight","bold")}
       else $with.setProperty("font-weight","");
      $with.setProperty("font-style","normal");
      s = "";
      if (1 in AFont.FStyle) s = "italic";
      if (2 in AFont.FStyle) {
        if (s !== "") s = s + " ";
        s = s + "underline";
      };
      if (3 in AFont.FStyle) {
        if (s !== "") s = s + " ";
        s = s + "line-through";
      };
      if (s !== "") {
        $with.setProperty("text-decoration",s)}
       else $with.removeProperty("text-decoration");
    };
  };
  this.ExtractKeyCode = function (AEvent) {
    var Result = 0;
    var VLocation = 0;
    var VKey = "";
    VLocation = AEvent.location;
    VKey = pas.SysUtils.LowerCase(AEvent.key);
    Result = -1;
    var $tmp = VKey;
    if ($tmp === "backspace") {
      Result = 8}
     else if ($tmp === "tab") {
      Result = 9}
     else if ($tmp === "enter") {
      Result = 13}
     else if ($tmp === "shift") {
      Result = 16}
     else if ($tmp === "control") {
      Result = 17}
     else if ($tmp === "alt") {
      Result = 18}
     else if ($tmp === "altgraph") {
      Result = 18}
     else if ($tmp === "pause") {
      Result = 19}
     else if ($tmp === "capslock") {
      Result = 20}
     else if ($tmp === "escape") {
      Result = 27}
     else if ($tmp === "pageup") {
      Result = 33}
     else if ($tmp === "pagedown") {
      Result = 34}
     else if ($tmp === "end") {
      Result = 35}
     else if ($tmp === "home") {
      Result = 36}
     else if ($tmp === "arrowleft") {
      Result = 37}
     else if ($tmp === "arrowup") {
      Result = 38}
     else if ($tmp === "arrowright") {
      Result = 39}
     else if ($tmp === "arrowdown") {
      Result = 40}
     else if ($tmp === "insert") {
      Result = 45}
     else if ($tmp === "delete") {
      Result = 46}
     else if ($tmp === "f1") {
      Result = 112}
     else if ($tmp === "f2") {
      Result = 113}
     else if ($tmp === "f3") {
      Result = 114}
     else if ($tmp === "f4") {
      Result = 115}
     else if ($tmp === "f5") {
      Result = 116}
     else if ($tmp === "f6") {
      Result = 117}
     else if ($tmp === "f7") {
      Result = 118}
     else if ($tmp === "f8") {
      Result = 119}
     else if ($tmp === "f9") {
      Result = 120}
     else if ($tmp === "f10") {
      Result = 121}
     else if ($tmp === "f11") {
      Result = 122}
     else if ($tmp === "f12") {
      Result = 123}
     else if ($tmp === "f13") {
      Result = 124}
     else if ($tmp === "f14") {
      Result = 125}
     else if ($tmp === "f15") {
      Result = 126}
     else if ($tmp === "f16") {
      Result = 127}
     else if ($tmp === "f17") {
      Result = 128}
     else if ($tmp === "f18") {
      Result = 129}
     else if ($tmp === "f19") {
      Result = 130}
     else if ($tmp === "f20") {
      Result = 131}
     else if ($tmp === "numlock") {
      Result = 144}
     else if ($tmp === "scrolllock") Result = 145;
    if (VLocation === 3) {
      var $tmp1 = VKey;
      if ($tmp1 === "0") {
        Result = 96}
       else if ($tmp1 === "1") {
        Result = 97}
       else if ($tmp1 === "2") {
        Result = 98}
       else if ($tmp1 === "3") {
        Result = 99}
       else if ($tmp1 === "4") {
        Result = 100}
       else if ($tmp1 === "5") {
        Result = 101}
       else if ($tmp1 === "6") {
        Result = 102}
       else if ($tmp1 === "7") {
        Result = 103}
       else if ($tmp1 === "8") {
        Result = 104}
       else if ($tmp1 === "9") {
        Result = 105}
       else if ($tmp1 === "*") {
        Result = 106}
       else if ($tmp1 === "+") {
        Result = 107}
       else if ($tmp1 === "-") {
        Result = 109}
       else if ($tmp1 === ",") {
        Result = 110}
       else if ($tmp1 === "\/") {
        Result = 111}
       else if ($tmp1 === ".") Result = 194;
    } else {
      var $tmp2 = VKey;
      if ($tmp2 === "0") {
        Result = 48}
       else if ($tmp2 === "1") {
        Result = 49}
       else if ($tmp2 === "2") {
        Result = 50}
       else if ($tmp2 === "3") {
        Result = 51}
       else if ($tmp2 === "4") {
        Result = 52}
       else if ($tmp2 === "5") {
        Result = 53}
       else if ($tmp2 === "6") {
        Result = 54}
       else if ($tmp2 === "7") {
        Result = 55}
       else if ($tmp2 === "8") {
        Result = 56}
       else if ($tmp2 === "9") {
        Result = 57}
       else if ($tmp2 === "ç") {
        Result = 63}
       else if ($tmp2 === "a") {
        Result = 65}
       else if ($tmp2 === "b") {
        Result = 66}
       else if ($tmp2 === "c") {
        Result = 67}
       else if ($tmp2 === "d") {
        Result = 68}
       else if ($tmp2 === "e") {
        Result = 69}
       else if ($tmp2 === "f") {
        Result = 70}
       else if ($tmp2 === "g") {
        Result = 71}
       else if ($tmp2 === "h") {
        Result = 72}
       else if ($tmp2 === "i") {
        Result = 73}
       else if ($tmp2 === "j") {
        Result = 74}
       else if ($tmp2 === "k") {
        Result = 75}
       else if ($tmp2 === "l") {
        Result = 76}
       else if ($tmp2 === "m") {
        Result = 77}
       else if ($tmp2 === "n") {
        Result = 78}
       else if ($tmp2 === "o") {
        Result = 79}
       else if ($tmp2 === "p") {
        Result = 80}
       else if ($tmp2 === "q") {
        Result = 81}
       else if ($tmp2 === "r") {
        Result = 82}
       else if ($tmp2 === "s") {
        Result = 83}
       else if ($tmp2 === "t") {
        Result = 84}
       else if ($tmp2 === "u") {
        Result = 85}
       else if ($tmp2 === "v") {
        Result = 86}
       else if ($tmp2 === "w") {
        Result = 87}
       else if ($tmp2 === "x") {
        Result = 88}
       else if ($tmp2 === "y") {
        Result = 89}
       else if ($tmp2 === "z") {
        Result = 90}
       else if ($tmp2 === "=") {
        Result = 187}
       else if ($tmp2 === ",") {
        Result = 188}
       else if ($tmp2 === "-") {
        Result = 189}
       else if ($tmp2 === ".") {
        Result = 190}
       else if ($tmp2 === "'") {
        Result = 192}
       else if ($tmp2 === "\/") {
        Result = 193}
       else if ($tmp2 === "]") {
        Result = 220}
       else if ($tmp2 === "[") Result = 221;
    };
    return Result;
  };
  this.ExtractKeyChar = function (AEvent) {
    var Result = "";
    var VKey = "";
    VKey = pas.SysUtils.LowerCase(AEvent.key);
    Result = "\x00";
    if (VKey.length === 1) {
      Result = VKey.charAt(0);
    } else {
      var $tmp = VKey;
      if ($tmp === "backspace") {
        Result = "\b"}
       else if ($tmp === "tab") {
        Result = "\t"}
       else if ($tmp === "enter") {
        Result = "\r"}
       else if ($tmp === "escape") Result = "\x1B";
    };
    return Result;
  };
  this.ExtractShiftState = function (AEvent) {
    var Result = {};
    Result = {};
    if (AEvent.altKey) {
      Result = rtl.unionSet(Result,rtl.createSet(1));
    };
    if (AEvent.ctrlKey) {
      Result = rtl.unionSet(Result,rtl.createSet(2));
    };
    if (AEvent.shiftKey) {
      Result = rtl.unionSet(Result,rtl.createSet(0));
    };
    return Result;
  };
  this.ExtractShiftState$1 = function (AEvent) {
    var Result = {};
    Result = {};
    if (AEvent.altKey) {
      Result = rtl.unionSet(Result,rtl.createSet(1));
    };
    if (AEvent.ctrlKey) {
      Result = rtl.unionSet(Result,rtl.createSet(2));
    };
    if (AEvent.shiftKey) {
      Result = rtl.unionSet(Result,rtl.createSet(0));
    };
    return Result;
  };
  this.ExtractMouseButton = function (AEvent) {
    var Result = 0;
    var $tmp = AEvent.button;
    if ($tmp === 0) {
      Result = 0}
     else if ($tmp === 1) {
      Result = 2}
     else if ($tmp === 2) {
      Result = 1}
     else {
      Result = 2;
    };
    return Result;
  };
  this.JSCursor = function (ACursor) {
    var Result = "";
    var $tmp = ACursor;
    if ($tmp === -1) {
      Result = "none"}
     else if ($tmp === -3) {
      Result = "crosshair"}
     else if ($tmp === -4) {
      Result = "text"}
     else if ($tmp === -22) {
      Result = "move"}
     else if ($tmp === -6) {
      Result = "nesw-resize"}
     else if ($tmp === -7) {
      Result = "ns-resize"}
     else if ($tmp === -8) {
      Result = "nwse-resize"}
     else if ($tmp === -9) {
      Result = "ew-resize"}
     else if ($tmp === -23) {
      Result = "nwse-resize"}
     else if ($tmp === -24) {
      Result = "ns-resize"}
     else if ($tmp === -25) {
      Result = "nesw-resize"}
     else if ($tmp === -26) {
      Result = "col-resize"}
     else if ($tmp === -27) {
      Result = "col-resize"}
     else if ($tmp === -28) {
      Result = "nesw-resize"}
     else if ($tmp === -29) {
      Result = "ns-resize"}
     else if ($tmp === -30) {
      Result = "nwse-resize"}
     else if ($tmp === -11) {
      Result = "wait"}
     else if ($tmp === -13) {
      Result = "no-drop"}
     else if ($tmp === -14) {
      Result = "col-resize"}
     else if ($tmp === -15) {
      Result = "row-resize"}
     else if ($tmp === -17) {
      Result = "progress"}
     else if ($tmp === -18) {
      Result = "not-allowed"}
     else if ($tmp === -19) {
      Result = "wait"}
     else if ($tmp === -20) {
      Result = "help"}
     else if ($tmp === -21) {
      Result = "pointer"}
     else {
      Result = "";
    };
    return Result;
  };
  $mod.$implcode = function () {
    $impl.CursorIdents$a$clone = function (a) {
      var b = [];
      b.length = 30;
      for (var c = 0; c < 30; c++) b[c] = pas.Classes.TIdentMapEntry.$clone(a[c]);
      return b;
    };
    $impl.CursorIdents = [pas.Classes.TIdentMapEntry.$clone({Value: 0, Name: "crDefault"}),pas.Classes.TIdentMapEntry.$clone({Value: -1, Name: "crNone"}),pas.Classes.TIdentMapEntry.$clone({Value: -2, Name: "crArrow"}),pas.Classes.TIdentMapEntry.$clone({Value: -3, Name: "crCross"}),pas.Classes.TIdentMapEntry.$clone({Value: -4, Name: "crIBeam"}),pas.Classes.TIdentMapEntry.$clone({Value: -6, Name: "crSizeNESW"}),pas.Classes.TIdentMapEntry.$clone({Value: -7, Name: "crSizeNS"}),pas.Classes.TIdentMapEntry.$clone({Value: -8, Name: "crSizeNWSE"}),pas.Classes.TIdentMapEntry.$clone({Value: -9, Name: "crSizeWE"}),pas.Classes.TIdentMapEntry.$clone({Value: -23, Name: "crSizeNW"}),pas.Classes.TIdentMapEntry.$clone({Value: -24, Name: "crSizeN"}),pas.Classes.TIdentMapEntry.$clone({Value: -25, Name: "crSizeNE"}),pas.Classes.TIdentMapEntry.$clone({Value: -26, Name: "crSizeW"}),pas.Classes.TIdentMapEntry.$clone({Value: -27, Name: "crSizeE"}),pas.Classes.TIdentMapEntry.$clone({Value: -28, Name: "crSizeSW"}),pas.Classes.TIdentMapEntry.$clone({Value: -29, Name: "crSizeS"}),pas.Classes.TIdentMapEntry.$clone({Value: -30, Name: "crSizeSE"}),pas.Classes.TIdentMapEntry.$clone({Value: -10, Name: "crUpArrow"}),pas.Classes.TIdentMapEntry.$clone({Value: -11, Name: "crHourGlass"}),pas.Classes.TIdentMapEntry.$clone({Value: -12, Name: "crDrag"}),pas.Classes.TIdentMapEntry.$clone({Value: -13, Name: "crNoDrop"}),pas.Classes.TIdentMapEntry.$clone({Value: -14, Name: "crHSplit"}),pas.Classes.TIdentMapEntry.$clone({Value: -15, Name: "crVSplit"}),pas.Classes.TIdentMapEntry.$clone({Value: -16, Name: "crMultiDrag"}),pas.Classes.TIdentMapEntry.$clone({Value: -17, Name: "crSQLWait"}),pas.Classes.TIdentMapEntry.$clone({Value: -18, Name: "crNo"}),pas.Classes.TIdentMapEntry.$clone({Value: -19, Name: "crAppStart"}),pas.Classes.TIdentMapEntry.$clone({Value: -20, Name: "crHelp"}),pas.Classes.TIdentMapEntry.$clone({Value: -21, Name: "crHandPoint"}),pas.Classes.TIdentMapEntry.$clone({Value: -22, Name: "crSizeAll"})];
    $impl.CursorToIdent = function (aCursor, aIdent) {
      var Result = false;
      Result = pas.Classes.IntToIdent(aCursor,aIdent,$impl.CursorIdents);
      return Result;
    };
    $impl.IdentToCursor = function (aIdent, aCursor) {
      var Result = false;
      Result = pas.Classes.IdentToInt(aIdent,aCursor,$impl.CursorIdents);
      return Result;
    };
  };
  $mod.$init = function () {
    pas.Classes.RegisterIntegerConsts($mod.$rtti["TCursor"],$impl.IdentToCursor,$impl.CursorToIdent);
  };
},["Forms"]);
rtl.module("p2jsres",["System","Types"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.TResourceSource = {"0": "rsJS", rsJS: 0, "1": "rsHTML", rsHTML: 1};
  rtl.recNewT(this,"TResourceInfo",function () {
    this.name = "";
    this.encoding = "";
    this.resourceunit = "";
    this.format = "";
    this.data = "";
    this.$eq = function (b) {
      return (this.name === b.name) && (this.encoding === b.encoding) && (this.resourceunit === b.resourceunit) && (this.format === b.format) && (this.data === b.data);
    };
    this.$assign = function (s) {
      this.name = s.name;
      this.encoding = s.encoding;
      this.resourceunit = s.resourceunit;
      this.format = s.format;
      this.data = s.data;
      return this;
    };
  });
  this.SetResourceSource = function (aSource) {
    var Result = 0;
    Result = $impl.gMode;
    $impl.gMode = aSource;
    return Result;
  };
  this.GetResourceInfo = function (aName, aInfo) {
    var Result = false;
    Result = $mod.GetResourceInfo$1($impl.gMode,aName,aInfo);
    return Result;
  };
  this.GetResourceInfo$1 = function (aSource, aName, aInfo) {
    var Result = false;
    var $tmp = aSource;
    if ($tmp === 0) {
      Result = $impl.GetRTLResourceInfo(aName,aInfo)}
     else if ($tmp === 1) Result = $impl.GetHTMLResourceInfo(aName,aInfo);
    return Result;
  };
  $mod.$implcode = function () {
    $impl.gMode = 0;
    $impl.GetRTLResourceInfo = function (aName, aInfo) {
      var Result = false;
      var RTLInfo = null;
      RTLInfo = rtl.getResource(pas.SysUtils.LowerCase(aName));
      Result = RTLInfo != null;
      if (Result) {
        aInfo.name = RTLInfo.name;
        aInfo.encoding = RTLInfo.encoding;
        aInfo.format = RTLInfo.format;
        aInfo.resourceunit = RTLInfo.unit;
        aInfo.data = RTLInfo.data;
      };
      return Result;
    };
    $impl.IDPrefix = "resource-";
    $impl.GetHTMLResourceInfo = function (aName, aInfo) {
      var Result = false;
      var el = null;
      var S = "";
      var I = 0;
      Result = false;
      if (!pas.JS.isDefined(document)) return Result;
      el = document.getElementById($impl.IDPrefix + pas.SysUtils.LowerCase(aName));
      Result = (el != null) && pas.SysUtils.SameText(el.tagName,"link");
      if (!Result) return Result;
      aInfo.name = pas.SysUtils.LowerCase(aName);
      aInfo.resourceunit = "" + el.dataset["unit"];
      S = el.href;
      S = pas.System.Copy(S,6,S.length - 5);
      I = pas.System.Pos(",",S);
      aInfo.data = pas.System.Copy(S,I + 1,S.length - 1);
      S = pas.System.Copy(S,1,I - 1);
      I = pas.System.Pos(";",S);
      if (I === 0) {
        aInfo.encoding = ""}
       else {
        aInfo.encoding = pas.System.Copy(S,I + 1,S.length - 1);
        S = pas.System.Copy(S,1,I - 1);
      };
      aInfo.format = S;
      return Result;
    };
  };
},["SysUtils","JS","Web"]);
rtl.module("WCLStrConsts",["System"],function () {
  "use strict";
  var $mod = this;
  $mod.$resourcestrings = {rsFormResourceSNotFoundForResourcelessFormsCreateNew: {org: "Form resource %s not found. For resourceless forms CreateNew constructor must be used."}, rsFormStreamingError: {org: 'Form streaming "%s" error: %s'}, rsFileButtonNoFileSelected: {org: "No file selected"}, rsErrUncaughtException: {org: "Uncaught exception of type %s: \n\n%s"}, rsErrUncaughtObject: {org: "Uncaught exception of type %s."}, rsNoTimers: {org: "No more timers available."}};
});
rtl.module("WResources",["System","Classes"],function () {
  "use strict";
  var $mod = this;
  this.InitResourceComponent = function (Instance, RootAncestor) {
    var Result = false;
    function InitComponent(ClassType) {
      var Result = false;
      var data = "";
      var ResName = "";
      var Stream = null;
      var BinStream = null;
      var Reader = null;
      var info = pas.p2jsres.TResourceInfo.$new();
      Result = false;
      if ((ClassType === pas.Classes.TComponent) || (ClassType === RootAncestor)) return Result;
      if (ClassType.$ancestor != null) Result = InitComponent(ClassType.$ancestor);
      Stream = null;
      ResName = ClassType.$module.$name;
      if (!pas.p2jsres.GetResourceInfo(ResName,info)) return Result;
      data = window.atob(info.data);
      if (data !== "") Stream = pas.Classes.TStringStream.$create("Create$2",[data]);
      if (!(Stream != null)) return Result;
      try {
        try {
          BinStream = pas.Classes.TMemoryStream.$create("Create");
          try {
            pas.Classes.ObjectTextToBinary(Stream,BinStream);
            BinStream.SetPosition(0);
            Reader = pas.Classes.TReader.$create("Create$1",[BinStream]);
            try {
              Reader.ReadRootComponent(Instance);
            } finally {
              Reader = rtl.freeLoc(Reader);
            };
          } finally {
            BinStream = rtl.freeLoc(BinStream);
          };
        } catch ($e) {
          if (pas.SysUtils.Exception.isPrototypeOf($e)) {
            var E = $e;
            pas.System.Writeln(pas.SysUtils.Format(rtl.getResStr(pas.WCLStrConsts,"rsFormStreamingError"),pas.System.VarRecs(18,ClassType.$classname,18,E.fMessage)));
            throw $e;
          } else throw $e
        };
      } finally {
        Stream = rtl.freeLoc(Stream);
      };
      Result = true;
      return Result;
    };
    if (rtl.neSet(rtl.intersectSet(Instance.FComponentState,rtl.createSet(0,9)),{})) {
      Result = InitComponent(Instance.$class.ClassType());
    } else try {
      Result = InitComponent(Instance.$class.ClassType());
    } finally {
    };
    return Result;
  };
  $mod.$init = function () {
    pas.Classes.RegisterInitComponentHandler(pas.Classes.TComponent,$mod.InitResourceComponent);
  };
},["Web","SysUtils","p2jsres","WCLStrConsts"]);
rtl.module("Forms",["System","Classes","SysUtils","Types","JS","Web","Graphics","Controls"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.TFormType = {"0": "ftModalForm", ftModalForm: 0, "1": "ftWindow", ftWindow: 1};
  this.TCloseAction = {"0": "caNone", caNone: 0, "1": "caHide", caHide: 1, "2": "caFree", caFree: 2};
  this.$rtti.$Enum("TCloseAction",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TCloseAction});
  this.$rtti.$MethodVar("TCloseEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["CloseAction",this.$rtti["TCloseAction"],1]]), methodkind: 0});
  this.$rtti.$MethodVar("TCloseQueryEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["CanClose",rtl.boolean,1]]), methodkind: 0});
  this.$rtti.$Int("TModalResult",{minvalue: -2147483648, maxvalue: 2147483647, ordtype: 4});
  rtl.createClass(this,"TCustomForm",pas.Controls.TCustomControl,function () {
    this.$init = function () {
      pas.Controls.TCustomControl.$init.call(this);
      this.FActiveControl = null;
      this.FAlphaBlend = false;
      this.FAlphaBlendValue = 0;
      this.FChildForm = null;
      this.FDesignTimePPI = 0;
      this.FFormType = 0;
      this.FKeyPreview = false;
      this.FModalResult = 0;
      this.FModalResultProc = null;
      this.FOverlay = null;
      this.FOnActivate = null;
      this.FOnClose = null;
      this.FOnCloseQuery = null;
      this.FOnCreate = null;
      this.FOnDeactivate = null;
      this.FOnDestroy = null;
      this.FOnHide = null;
      this.FOnResize$1 = null;
      this.FOnScroll$1 = null;
      this.FOnShow = null;
      this.fFormBorderStyle = 0;
    };
    this.$final = function () {
      this.FActiveControl = undefined;
      this.FChildForm = undefined;
      this.FModalResultProc = undefined;
      this.FOverlay = undefined;
      this.FOnActivate = undefined;
      this.FOnClose = undefined;
      this.FOnCloseQuery = undefined;
      this.FOnCreate = undefined;
      this.FOnDeactivate = undefined;
      this.FOnDestroy = undefined;
      this.FOnHide = undefined;
      this.FOnResize$1 = undefined;
      this.FOnScroll$1 = undefined;
      this.FOnShow = undefined;
      pas.Controls.TCustomControl.$final.call(this);
    };
    this.AppClickHandler = function (aEvent) {
      var Result = false;
      var i = 0;
      for (var $l = 0, $end = this.GetControlCount() - 1; $l <= $end; $l++) {
        i = $l;
        if (pas.Controls.TCustomPopupMenu.isPrototypeOf(this.GetControl(i))) this.GetControl(i).SetVisible(false);
      };
      return Result;
    };
    this.SetActiveControl = function (AValue) {
      if (this.FActiveControl !== AValue) {
        this.FActiveControl = AValue;
      };
    };
    this.SetAlphaBlend = function (AValue) {
      if (this.FAlphaBlend !== AValue) {
        this.FAlphaBlend = AValue;
        this.Changed();
      };
    };
    this.SetAlphaBlendValue = function (AValue) {
      if (this.FAlphaBlendValue !== AValue) {
        this.FAlphaBlendValue = AValue;
        this.Changed();
      };
    };
    this.SetFormBorderStyle = function (AValue) {
      var bs = pas.Controls.TFormBorderStyle.bsNone;
      if (this.fFormBorderStyle === AValue) return;
      this.fFormBorderStyle = AValue;
      if (AValue in rtl.createSet(null,0,1)) {
        bs = AValue}
       else bs = 0;
      pas.Controls.TControl.SetBorderStyle.call(this,bs);
    };
    this.SetModalResult = function (AValue) {
      if (this.FModalResult !== AValue) {
        this.FModalResult = AValue;
        if ((this.FModalResult !== 0) && (this.FModalResultProc != null)) {
          this.Close();
        };
      };
    };
    this.Activate = function () {
      if (this.FOnActivate != null) {
        this.FOnActivate(this);
      };
    };
    this.Deactivate = function () {
      if (this.FOnDeactivate != null) {
        this.FOnDeactivate(this);
      };
    };
    this.DoClose = function (CloseAction) {
      if (this.FOnDeactivate != null) {
        this.FOnDeactivate(this);
      };
    };
    this.DoCreate = function () {
      if (this.FOnCreate != null) {
        this.FOnCreate(this);
      };
    };
    this.DoDestroy = function () {
      if (this.FOnDestroy != null) {
        this.FOnDestroy(this);
      };
    };
    this.DoHide = function () {
      if (this.FOnHide != null) {
        this.FOnHide(this);
      };
    };
    this.DoResize = function () {
      pas.Controls.TControl.DoResize.call(this);
      if (this.FOnResize$1 != null) {
        this.FOnResize$1(this);
      };
    };
    this.DoShow = function () {
      if (this.FOnShow != null) {
        this.FOnShow(this);
      };
    };
    this.HandleEnter = function (AEvent) {
      var Result = false;
      var VControl = null;
      Result = pas.Controls.TWinControl.HandleEnter.call(this,AEvent);
      if ((this.FChildForm != null) && (this.FChildForm.FFormType === 0)) {
        this.FChildForm.Show();
      } else {
        if (this.FActiveControl != null) {
          VControl = this.FActiveControl;
        } else {
          VControl = this.FindFocusControl(null,0);
        };
        this.FocusControl(VControl);
        this.Activate();
      };
      return Result;
    };
    this.HandleExit = function (AEvent) {
      var Result = false;
      Result = pas.Controls.TWinControl.HandleExit.call(this,AEvent);
      this.Deactivate();
      return Result;
    };
    this.Changed = function () {
      pas.Controls.TControl.Changed.call(this);
      if (!this.IsUpdating() && !(0 in this.FComponentState)) {
        var $with = this.FHandleElement;
        $with.style.setProperty("outline","none");
        if (this.FAlphaBlend) {
          $with.style.setProperty("opacity",pas.SysUtils.FloatToStr(rtl.trunc(this.FAlphaBlendValue / 255)));
        } else {
          $with.style.removeProperty("opacity");
        };
        $with.style.setProperty("overflow","auto");
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("div");
      return Result;
    };
    this.ProcessResource = function () {
      if (!pas.WResources.InitResourceComponent(this,$mod.TWForm)) throw pas.Classes.EResNotFound.$create("CreateFmt",[rtl.getResStr(pas.WCLStrConsts,"rsFormResourceSNotFoundForResourcelessFormsCreateNew"),pas.System.VarRecs(18,this.$classname)]);
    };
    this.GetControlClassDefaultSize = function () {
      var Result = pas.Types.TSize.$new();
      Result.cx = 320;
      Result.cy = 240;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      this.CreateNew(AOwner,1);
      if ((this.$class.ClassType() !== $mod.TWForm) && !(4 in this.FComponentState)) {
        this.ProcessResource();
      };
      document.addEventListener("click",rtl.createCallback(this,"AppClickHandler"));
      return this;
    };
    this.CreateNew = function (AOwner, Num) {
      pas.Controls.TControl.Create$1.call(this,AOwner);
      this.FActiveControl = null;
      this.FAlphaBlend = false;
      this.FAlphaBlendValue = 255;
      this.FDesignTimePPI = 96;
      this.FChildForm = null;
      this.FFormType = 1;
      this.FKeyPreview = false;
      this.FModalResult = 0;
      this.FModalResultProc = null;
      this.FOverlay = null;
      this.SetFormBorderStyle(2);
      this.BeginUpdate();
      try {
        this.SetColor(16777215);
        this.SetParentFont(false);
        this.SetParentShowHint(false);
        this.SetVisible(false);
        var $with = this.$class.GetControlClassDefaultSize();
        this.SetBounds(0,0,$with.cx,$with.cy);
      } finally {
        this.EndUpdate();
      };
      return this;
    };
    this.Destroy = function () {
      this.FActiveControl = null;
      this.FChildForm = null;
      document.removeEventListener("click",rtl.createCallback(this,"AppClickHandler"));
      pas.Controls.TCustomControl.Destroy.call(this);
    };
    this.AfterConstruction = function () {
      pas.System.TObject.AfterConstruction.call(this);
      $mod.Application().UpdateMainForm(this);
      $mod.Application().RegisterModule(this);
      this.Loaded();
      this.DoCreate();
    };
    this.BeforeDestruction = function () {
      pas.Classes.TComponent.BeforeDestruction.call(this);
      $mod.Application().UnRegisterModule(this);
      this.DoDestroy();
    };
    this.Close = function () {
      var VAction = 0;
      var VIndex = 0;
      var VOwnerForm = null;
      var VModule = null;
      if (this.CloseQuery()) {
        VAction = 1;
        this.DoClose({get: function () {
            return VAction;
          }, set: function (v) {
            VAction = v;
          }});
        if (VAction !== 0) {
          if ($mod.Application().FMainForm === this) {
            $mod.Application().Terminate();
          } else {
            this.Hide();
            if (this.FFormType === 0) {
              if ((this.FOwner != null) && $mod.TCustomForm.isPrototypeOf(this.FOwner)) {
                VOwnerForm = this.FOwner;
                VOwnerForm.FChildForm = null;
                if (VOwnerForm.FOverlay != null) {
                  VOwnerForm.FOverlay.$destroy("Destroy");
                  VOwnerForm.FOverlay = null;
                };
                VOwnerForm.Show();
              };
              if (this.FModalResultProc != null) {
                this.FModalResultProc(this,this.FModalResult);
              };
            } else {
              for (var $l = $mod.Application().GetModuleCount() - 1; $l >= 0; $l--) {
                VIndex = $l;
                VModule = $mod.Application().GetModule(VIndex);
                if ((VModule != null) && VModule.FVisible && (VModule !== this) && VModule.$class.InheritsFrom($mod.TCustomForm)) {
                  VModule.Show();
                  return;
                };
              };
              if ($mod.Application().FMainForm != null) {
                $mod.Application().FMainForm.Show();
              };
            };
          };
        };
      };
    };
    this.CloseQuery = function () {
      var Result = false;
      Result = true;
      if (this.FOnCloseQuery != null) {
        this.FOnCloseQuery(this,{get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }});
      };
      return Result;
    };
    this.FocusControl = function (AControl) {
      if ((AControl != null) && AControl.CanSetFocus()) {
        AControl.SetFocus();
      };
    };
    this.Hide = function () {
      this.SetVisible(false);
      this.DoHide();
    };
    this.Loaded = function () {
      pas.Controls.TControl.Loaded.call(this);
    };
    this.Resize = function () {
      var VHeight = 0;
      var VLeft = 0;
      var VTop = 0;
      var VWidth = 0;
      var VWindowHeight = 0;
      var VWindowWidth = 0;
      VWindowWidth = window.innerWidth;
      VWindowHeight = window.innerHeight;
      var $tmp = this.FFormType;
      if ($tmp === 0) {
        VWidth = this.FWidth;
        VHeight = this.FHeight;
        VLeft = rtl.trunc((VWindowWidth - VWidth) / 2);
        VTop = rtl.trunc((VWindowHeight - VHeight) / 2);
        this.SetBounds(VLeft,VTop,VWidth,VHeight);
      } else if ($tmp === 1) {
        this.SetBounds(0,0,VWindowWidth,VWindowHeight);
      };
      this.DoResize();
    };
    this.Show = function () {
      $mod.Application().FActiveForm = this;
      $mod.Application().SetTitle(this.GetText());
      this.BeginUpdate();
      try {
        this.SetVisible(true);
        this.Resize();
      } finally {
        this.EndUpdate();
      };
      this.BringToFront();
      this.SetFocus();
      this.DoShow();
    };
    this.ShowModal = function (AModalResultProc) {
      var VForm = null;
      if (!(this.FOwner != null)) {
        throw new Error("Owner not found.");
      };
      if (!$mod.TCustomForm.isPrototypeOf(this.FOwner)) {
        throw new Error("Invalid owner.");
      };
      VForm = this.FOwner;
      if (VForm.FChildForm != null) {
        throw new Error("Modal form already exists.");
      };
      VForm.FChildForm = this;
      VForm.FOverlay = $impl.TOverlay.$create("Create$1",[VForm]);
      this.FFormType = 0;
      this.FModalResult = 0;
      if (AModalResultProc != null) {
        this.FModalResultProc = AModalResultProc;
      } else {
        this.FModalResultProc = $impl.DefaultModalProc;
      };
      this.Show();
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  rtl.createClass(this,"TApplication",pas.Classes.TComponent,function () {
    this.$init = function () {
      pas.Classes.TComponent.$init.call(this);
      this.FModules = null;
      this.FActiveForm = null;
      this.FMainForm = null;
      this.FStopOnException = false;
      this.FTerminated = false;
      this.FTitle = "";
      this.FOnResize = null;
      this.FOnUnload = null;
    };
    this.$final = function () {
      this.FModules = undefined;
      this.FActiveForm = undefined;
      this.FMainForm = undefined;
      this.FOnResize = undefined;
      this.FOnUnload = undefined;
      pas.Classes.TComponent.$final.call(this);
    };
    this.GetApplicatioName = function () {
      var Result = "";
      Result = window.location.pathname;
      return Result;
    };
    this.GetModule = function (AIndex) {
      var Result = null;
      Result = rtl.getObject(this.FModules[AIndex]);
      return Result;
    };
    this.GetModuleCount = function () {
      var Result = 0;
      Result = this.FModules.length;
      return Result;
    };
    this.SetTitle = function (AValue) {
      if (this.FTitle !== AValue) {
        this.FTitle = AValue;
        document.title = this.FTitle;
      };
    };
    this.DoResize = function () {
      if (this.FOnResize != null) {
        this.FOnResize(this);
      };
    };
    this.DoUnload = function () {
      if (this.FOnUnload != null) {
        this.FOnUnload(this);
      };
    };
    this.LoadIcon = function () {
      var $with = document.head.appendChild(document.createElement("link"));
      $with.setAttribute("rel","icon");
      $with.setAttribute("type","image\/icon");
      $with.setAttribute("href",this.GetApplicatioName().replace("html","ico"));
    };
    this.RegisterHandleEvents = function () {
      window.addEventListener("error",rtl.createCallback(this,"HandleError"));
      window.addEventListener("resize",rtl.createSafeCallback(this,"HandleResize"));
      window.addEventListener("unload",rtl.createCallback(this,"HandleUnload"));
    };
    this.UnRegisterHandleEvents = function () {
      window.removeEventListener("error",rtl.createCallback(this,"HandleError"));
      window.removeEventListener("resize",rtl.createSafeCallback(this,"HandleResize"));
      window.removeEventListener("unload",rtl.createCallback(this,"HandleUnload"));
    };
    var CLE = pas.System.LineEnding;
    var CError = "Error Message: %s " + CLE + "Line Nro: %d " + CLE + "Column Nro: %d " + CLE;
    this.HandleError = function (AEvent) {
      var Result = false;
      if (AEvent.message.toLowerCase().indexOf("script error",0) > -1) {
        window.alert("Script Error: See Browser Console for Detail");
      } else {
        window.alert(pas.SysUtils.Format(CError,pas.System.VarRecs(18,AEvent.message,0,AEvent.lineno,0,AEvent.colno)));
      };
      if (this.FStopOnException) {
        this.Terminate();
      };
      AEvent.stopPropagation();
      Result = false;
      return Result;
    };
    this.HandleResize = function (AEvent) {
      var Result = false;
      var VControl = null;
      var VIndex = 0;
      AEvent.stopPropagation();
      this.DoResize();
      Result = true;
      for (var $l = 0, $end = this.FModules.length - 1; $l <= $end; $l++) {
        VIndex = $l;
        VControl = rtl.getObject(this.FModules[VIndex]);
        if ((VControl != null) && VControl.FVisible && VControl.$class.InheritsFrom($mod.TCustomForm)) {
          VControl.Resize();
        };
      };
      return Result;
    };
    this.HandleUnload = function (AEvent) {
      var Result = false;
      AEvent.stopPropagation();
      Result = true;
      try {
        this.DoUnload();
      } finally {
        this.Terminate();
      };
      return Result;
    };
    this.HandleException = function (AException) {
      if (pas.SysUtils.Exception.isPrototypeOf(AException)) {
        window.alert(pas.SysUtils.Format(rtl.getResStr(pas.WCLStrConsts,"rsErrUncaughtException"),pas.System.VarRecs(18,AException.$classname,18,AException.fMessage)));
      } else {
        window.alert(pas.SysUtils.Format(rtl.getResStr(pas.WCLStrConsts,"rsErrUncaughtObject"),pas.System.VarRecs(18,AException.$classname)));
      };
      if (this.FStopOnException) this.Terminate();
    };
    this.Create$1 = function (AOwner) {
      pas.Classes.TComponent.Create$1.call(this,AOwner);
      pas.p2jsres.SetResourceSource(0);
      pas.SysUtils.SetOnUnCaughtExceptionHandler($impl.DoUncaughtPascalException);
      rtl.showUncaughtExceptions=true;
      this.FModules = new Array();
      this.FMainForm = null;
      this.FStopOnException = true;
      this.FTerminated = false;
      this.FTitle = "";
      return this;
    };
    this.Destroy = function () {
      this.FModules.length = 0;
      pas.Classes.TComponent.Destroy.call(this);
    };
    this.CreateForm = function (AInstanceClass, AReference) {
      try {
        AReference.set(AInstanceClass.$create("Create$1",[this]));
      } catch ($e) {
        AReference.set(null);
        throw $e;
      };
    };
    this.Initialize = function () {
    };
    this.Run = function () {
      this.RegisterHandleEvents();
      this.LoadIcon();
      if (this.FMainForm != null) {
        this.FMainForm.Show();
      };
    };
    this.Terminate = function () {
      var VModule = null;
      var VIndex = 0;
      if (!this.FTerminated) {
        this.UnRegisterHandleEvents();
        this.FTerminated = true;
        for (var $l = this.FModules.length - 1; $l >= 0; $l--) {
          VIndex = $l;
          VModule = rtl.getObject(this.FModules[VIndex]);
          if (VModule != null) {
            VModule.$destroy("Destroy");
            VModule = null;
          };
        };
      };
    };
    this.UpdateMainForm = function (AForm) {
      if (!(this.FMainForm != null)) {
        this.FMainForm = AForm;
        this.FActiveForm = AForm;
      };
    };
    this.RegisterModule = function (AModule) {
      if (AModule != null) {
        if (this.FModules.indexOf(AModule) === -1) {
          this.FModules.push(AModule);
          if (!document.body.contains(AModule.FHandleElement)) {
            document.body.appendChild(AModule.FHandleElement);
          };
        };
      };
    };
    this.UnRegisterModule = function (AModule) {
      var VIndex = 0;
      if (AModule != null) {
        VIndex = this.FModules.indexOf(AModule);
        if (VIndex >= 0) {
          this.FModules.splice(VIndex,1);
          if (document.body.contains(AModule.FHandleElement)) {
            document.body.removeChild(AModule.FHandleElement);
          };
        };
      };
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  rtl.createClass(this,"TWForm",this.TCustomForm,function () {
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("ActiveControl",2,pas.Controls.$rtti["TWinControl"],"FActiveControl","SetActiveControl");
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("AlphaBlend",2,rtl.boolean,"FAlphaBlend","SetAlphaBlend");
    $r.addProperty("AlphaBlendValue",2,rtl.byte,"FAlphaBlendValue","SetAlphaBlendValue");
    $r.addProperty("Caption",3,pas.Controls.$rtti["TCaption"],"GetText","SetText");
    $r.addProperty("ClientHeight",3,rtl.nativeint,"GetClientHeight","SetClientHeight");
    $r.addProperty("ClientWidth",3,rtl.nativeint,"GetClientWidth","SetClientWidth");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("DesignTimePPI",0,rtl.longint,"FDesignTimePPI","FDesignTimePPI");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("KeyPreview",0,rtl.boolean,"FKeyPreview","FKeyPreview");
    $r.addProperty("PopupMenu",2,pas.Controls.$rtti["TCustomPopupMenu"],"FPopupMenu","SetPopupMenu");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("OnActivate",0,pas.Classes.$rtti["TNotifyEvent"],"FOnActivate","FOnActivate");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnClose",0,$mod.$rtti["TCloseEvent"],"FOnClose","FOnClose");
    $r.addProperty("OnCloseQuery",0,$mod.$rtti["TCloseQueryEvent"],"FOnCloseQuery","FOnCloseQuery");
    $r.addProperty("OnCreate",0,pas.Classes.$rtti["TNotifyEvent"],"FOnCreate","FOnCreate");
    $r.addProperty("OnDblClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnDeactivate",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDeactivate","FOnDeactivate");
    $r.addProperty("OnDestroy",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDestroy","FOnDestroy");
    $r.addProperty("OnHide",0,pas.Classes.$rtti["TNotifyEvent"],"FOnHide","FOnHide");
    $r.addProperty("OnKeyDown",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyDown","FOnKeyDown");
    $r.addProperty("OnKeyPress",0,pas.Controls.$rtti["TKeyPressEvent"],"FOnKeyPress","FOnKeyPress");
    $r.addProperty("OnKeyUp",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
    $r.addProperty("OnResize",0,pas.Classes.$rtti["TNotifyEvent"],"FOnResize$1","FOnResize$1");
    $r.addProperty("OnScroll",0,pas.Classes.$rtti["TNotifyEvent"],"FOnScroll$1","FOnScroll$1");
    $r.addProperty("OnShow",0,pas.Classes.$rtti["TNotifyEvent"],"FOnShow","FOnShow");
  });
  this.Application = function () {
    var Result = null;
    if (!($impl.VAppInstance != null)) {
      $impl.VAppInstance = $mod.TApplication.$create("Create$1",[null]);
    };
    Result = $impl.VAppInstance;
    return Result;
  };
  $mod.$implcode = function () {
    $impl.DefaultModalProc = function (Sender, ModalResult) {
      if (Sender != null) {
        Sender.$destroy("Destroy");
        Sender = null;
      };
    };
    $impl.VAppInstance = null;
    rtl.createClass($impl,"TOverlay",pas.System.TObject,function () {
      this.$init = function () {
        pas.System.TObject.$init.call(this);
        this.FForm = null;
        this.FHandleElement = null;
      };
      this.$final = function () {
        this.FForm = undefined;
        this.FHandleElement = undefined;
        pas.System.TObject.$final.call(this);
      };
      this.Create$1 = function (AForm) {
        this.FForm = AForm;
        if (this.FForm != null) {
          this.FHandleElement = document.createElement("div");
          var $with = this.FHandleElement;
          $with.style.setProperty("left","0px");
          $with.style.setProperty("top","0px");
          $with.style.setProperty("height","100%");
          $with.style.setProperty("width","100%");
          $with.style.setProperty("background","rgba(0, 0, 0, 0.6)");
          $with.style.setProperty("position","absolute");
          $with.style.setProperty("overflow","hidden");
          this.FForm.FHandleElement.appendChild(this.FHandleElement);
        };
        return this;
      };
      this.Destroy = function () {
        if (this.FForm != null) {
          this.FForm.FHandleElement.removeChild(this.FHandleElement);
        };
        pas.System.TObject.Destroy.call(this);
      };
    });
    $impl.DoUncaughtPascalException = function (E) {
      $mod.Application().HandleException(E);
    };
  };
},["WResources","WCLStrConsts","p2jsres"]);
rtl.module("WebExtra",["System","JS","Classes","SysUtils","Web"],function () {
  "use strict";
  var $mod = this;
});
rtl.module("StdCtrls",["System","Classes","SysUtils","Types","Web","WebExtra","Graphics","Controls","Forms"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.TEditCharCase = {"0": "ecNormal", ecNormal: 0, "1": "ecUppercase", ecUppercase: 1, "2": "ecLowerCase", ecLowerCase: 2};
  this.$rtti.$Enum("TEditCharCase",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TEditCharCase});
  rtl.createClass(this,"TCustomComboBox",pas.Controls.TWinControl,function () {
    this.$init = function () {
      pas.Controls.TWinControl.$init.call(this);
      this.FDropDownCount = 0;
      this.FItemHeight = 0;
      this.FItemIndex = 0;
      this.FItems = null;
      this.FOnChange = null;
      this.FSorted = false;
      this.FSelectElement = null;
    };
    this.$final = function () {
      this.FItems = undefined;
      this.FOnChange = undefined;
      this.FSelectElement = undefined;
      pas.Controls.TWinControl.$final.call(this);
    };
    this.SetItemHeight = function (AValue) {
      if (this.FItemHeight !== AValue) {
        this.FItemHeight = AValue;
        this.Change();
      };
    };
    this.SetItemIndex = function (AValue) {
      if ((AValue > -1) && (AValue < this.FItems.GetCount())) {
        this.FItemIndex = AValue;
        this.Changed();
      };
    };
    this.SetItems = function (AValue) {
      this.FItems.Assign(AValue);
      this.Changed();
    };
    this.ItemsChange = function (ASender) {
      this.Changed();
    };
    this.Change = function () {
      if (this.FOnChange != null) {
        this.FOnChange(this);
      };
    };
    this.HandleChange = function (AEvent) {
      var Result = false;
      AEvent.stopPropagation();
      this.FItemIndex = this.FHandleElement.selectedIndex;
      this.Change();
      Result = true;
      return Result;
    };
    this.Changed = function () {
      var VIndex = 0;
      var VOptionElement = null;
      var VValue = "";
      pas.Controls.TControl.Changed.call(this);
      if (!this.IsUpdating() && !(0 in this.FComponentState)) {
        this.FHandleElement.style.removeProperty("font");
        this.FSelectElement.style.setProperty("width","100%");
        this.FSelectElement.style.setProperty("height","100%");
        this.FSelectElement.style.setProperty("border-style","none");
        this.FSelectElement.style.setProperty("box-sizing","border-box");
        for (var $l = this.FSelectElement.length - 1; $l >= 0; $l--) {
          VIndex = $l;
          this.FSelectElement.remove(VIndex);
        };
        for (var $l1 = 0, $end = this.FItems.GetCount() - 1; $l1 <= $end; $l1++) {
          VIndex = $l1;
          VValue = this.FItems.Get(VIndex);
          VOptionElement = document.createElement("option");
          VOptionElement.value = VValue;
          VOptionElement.text = VValue;
          VOptionElement.selected = VIndex === this.FItemIndex;
          this.FSelectElement.add(VOptionElement);
        };
        if (this.FItemIndex < 0) {
          VOptionElement = document.createElement("option");
          VOptionElement.value = "";
          VOptionElement.text = "";
          VOptionElement.selected = true;
          VOptionElement.disabled = true;
          VOptionElement.style.setProperty("display","none");
          this.FSelectElement.add(VOptionElement);
        };
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("div");
      return Result;
    };
    this.CreateSelectElement = function () {
      var Result = null;
      Result = this.FHandleElement.appendChild(document.createElement("select"));
      return Result;
    };
    this.RegisterHandleEvents = function () {
      pas.Controls.TWinControl.RegisterHandleEvents.call(this);
      var $with = this.FHandleElement;
      $with.addEventListener("change",rtl.createSafeCallback(this,"HandleChange"));
    };
    this.UnRegisterHandleEvents = function () {
      pas.Controls.TWinControl.UnRegisterHandleEvents.call(this);
      var $with = this.FHandleElement;
      $with.removeEventListener("change",rtl.createSafeCallback(this,"HandleChange"));
    };
    this.CheckChildClassAllowed = function (AChildClass) {
      var Result = false;
      Result = false;
      return Result;
    };
    this.RealGetText = function () {
      var Result = "";
      Result = this.FItems.Get(this.FItemIndex);
      return Result;
    };
    this.RealSetText = function (AValue) {
      var VIndex = 0;
      VIndex = this.FItems.IndexOf(AValue);
      if ((VIndex > -1) && (VIndex < this.FItems.GetCount())) {
        this.FItemIndex = VIndex;
        this.Changed();
      };
    };
    this.GetControlClassDefaultSize = function () {
      var Result = pas.Types.TSize.$new();
      Result.cx = 100;
      Result.cy = 25;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.Controls.TControl.Create$1.call(this,AOwner);
      this.FSelectElement = this.CreateSelectElement();
      this.FDropDownCount = 8;
      this.FItemHeight = 0;
      this.FItemIndex = -1;
      this.FItems = pas.Classes.TStringList.$create("Create$1");
      this.FItems.FOnChange = rtl.createCallback(this,"ItemsChange");
      this.FSorted = false;
      this.BeginUpdate();
      try {
        var $with = this.$class.GetControlClassDefaultSize();
        this.SetBounds(0,0,$with.cx,$with.cy);
      } finally {
        this.EndUpdate();
      };
      return this;
    };
    this.Destroy = function () {
      this.FItems.$destroy("Destroy");
      this.FItems = null;
      pas.Controls.TControl.Destroy.call(this);
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  this.$rtti.$MethodVar("TSelectionChangeEvent",{procsig: rtl.newTIProcSig([["Sender",pas.System.$rtti["TObject"]],["User",rtl.boolean]]), methodkind: 0});
  rtl.createClass(this,"TCustomListBox",pas.Controls.TWinControl,function () {
    this.$init = function () {
      pas.Controls.TWinControl.$init.call(this);
      this.FItemHeight = 0;
      this.FItemIndex = 0;
      this.FItems = null;
      this.FMultiSelect = false;
      this.FSelectionChanged = false;
      this.FSelected = [];
      this.FSorted = false;
      this.FOnSelectionChange = null;
    };
    this.$final = function () {
      this.FItems = undefined;
      this.FSelected = undefined;
      this.FOnSelectionChange = undefined;
      pas.Controls.TWinControl.$final.call(this);
    };
    this.SetItemHeight = function (AValue) {
      if (this.FItemHeight !== AValue) {
        this.FItemHeight = AValue;
        this.Changed();
      };
    };
    this.SetItemIndex = function (AValue) {
      if ((AValue > -1) && (AValue < this.FItems.GetCount())) {
        this.BeginUpdate();
        try {
          if (this.FMultiSelect) this.ClearSelection();
          this.FItemIndex = AValue;
          this.Changed();
        } finally {
          this.EndUpdate();
        };
      };
    };
    this.SetItems = function (AValue) {
      this.FItems.Assign(AValue);
      this.Changed();
    };
    this.SetMultiSelect = function (AValue) {
      if (this.FMultiSelect !== AValue) {
        this.ClearSelection();
        this.FMultiSelect = AValue;
        if (!(0 in this.FComponentState)) this.FSelectionChanged = true;
        this.Changed();
      };
    };
    this.SetSelected = function (Index, AValue) {
      var i = 0;
      if (Index > (rtl.length(this.FSelected) - 1)) throw pas.Classes.EListError.$create("CreateFmt",[rtl.getResStr(pas.RTLConsts,"SListIndexError"),pas.System.VarRecs(0,Index)]);
      if (AValue && !this.FMultiSelect) {
        for (var $l = 0, $end = rtl.length(this.FSelected) - 1; $l <= $end; $l++) {
          i = $l;
          if (this.FSelected[i]) this.FSelected[i] = false;
        };
      };
      this.FSelected[Index] = AValue;
      if (AValue) {
        this.FItemIndex = Index}
       else {
        this.FItemIndex = -1;
        if (this.FMultiSelect) {
          for (var $l1 = 0, $end1 = rtl.length(this.FSelected) - 1; $l1 <= $end1; $l1++) {
            i = $l1;
            if (this.FSelected[i]) {
              this.FItemIndex = i;
              break;
            };
          };
        };
      };
      if (!(0 in this.FComponentState)) this.FSelectionChanged = true;
      this.Changed();
    };
    this.ItemsChanged = function (ASender) {
      if (rtl.length(this.FSelected) !== this.FItems.GetCount()) this.FSelected = rtl.arraySetLength(this.FSelected,false,this.FItems.GetCount());
      this.Changed();
    };
    this.SelectionChange = function (AUser) {
      if (this.FOnSelectionChange != null) this.FOnSelectionChange(this,AUser);
    };
    this.HandleChange = function (AEvent) {
      var Result = false;
      var i = 0;
      AEvent.stopPropagation();
      var $with = this.FHandleElement;
      this.FItemIndex = $with.selectedIndex;
      for (var $l = 0, $end = $with.length - 1; $l <= $end; $l++) {
        i = $l;
        this.FSelected[i] = $with.item(i).selected;
      };
      this.SelectionChange(true);
      Result = true;
      return Result;
    };
    this.Changed = function () {
      var idx = 0;
      var v = "";
      var opt = null;
      pas.Controls.TControl.Changed.call(this);
      if (!this.IsUpdating() && !(0 in this.FComponentState)) {
        if (this.FSelectionChanged) {
          this.SelectionChange(false);
          this.FSelectionChanged = false;
        };
        var $with = this.FHandleElement;
        $with.style.setProperty("overflow","auto");
        $with.multiple = this.FMultiSelect;
        $with.size = 2;
        for (var $l = this.FHandleElement.length - 1; $l >= 0; $l--) {
          idx = $l;
          $with.remove(idx);
        };
        for (var $l1 = 0, $end = this.FItems.GetCount() - 1; $l1 <= $end; $l1++) {
          idx = $l1;
          v = this.FItems.Get(idx);
          opt = document.createElement("option");
          opt.value = v;
          opt.text = v;
          if (this.FMultiSelect) {
            opt.selected = this.FSelected[idx]}
           else opt.selected = idx === this.FItemIndex;
          $with.add(opt);
        };
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("select");
      return Result;
    };
    this.RegisterHandleEvents = function () {
      pas.Controls.TWinControl.RegisterHandleEvents.call(this);
      var $with = this.FHandleElement;
      $with.addEventListener("change",rtl.createSafeCallback(this,"HandleChange"));
    };
    this.UnRegisterHandleEvents = function () {
      pas.Controls.TWinControl.UnRegisterHandleEvents.call(this);
      var $with = this.FHandleElement;
      $with.removeEventListener("change",rtl.createSafeCallback(this,"HandleChange"));
    };
    this.CheckChildClassAllowed = function (AChildClass) {
      var Result = false;
      Result = false;
      return Result;
    };
    this.GetControlClassDefaultSize = function () {
      var Result = pas.Types.TSize.$new();
      Result.cx = 100;
      Result.cy = 70;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.Controls.TControl.Create$1.call(this,AOwner);
      this.FItemHeight = 0;
      this.FItemIndex = -1;
      this.FItems = pas.Classes.TStringList.$create("Create$1");
      this.FItems.FOnChange = rtl.createCallback(this,"ItemsChanged");
      this.FMultiSelect = false;
      this.FSorted = false;
      this.BeginUpdate();
      try {
        var $with = this.$class.GetControlClassDefaultSize();
        this.SetBounds(0,0,$with.cx,$with.cy);
      } finally {
        this.EndUpdate();
      };
      return this;
    };
    this.Destroy = function () {
      rtl.free(this,"FItems");
      pas.Controls.TControl.Destroy.call(this);
    };
    this.ClearSelection = function () {
      var i = 0;
      if (this.FMultiSelect) {
        this.BeginUpdate();
        try {
          for (var $l = 0, $end = this.FItems.GetCount() - 1; $l <= $end; $l++) {
            i = $l;
            this.SetSelected(i,false);
          };
        } finally {
          this.EndUpdate();
        };
      } else this.SetItemIndex(-1);
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  rtl.createClass(this,"TCustomEdit",pas.Controls.TWinControl,function () {
    this.$init = function () {
      pas.Controls.TWinControl.$init.call(this);
      this.FAlignment = 0;
      this.FCharCase = 0;
      this.FInputElement = null;
      this.FMaxLength = 0;
      this.FModified = false;
      this.FPasswordChar = "";
      this.FPattern = "";
      this.FReadOnly = false;
      this.FRequired = false;
      this.FSelLength = 0;
      this.FSelStart = 0;
      this.FText = "";
      this.FTextHint = "";
      this.FOnChange = null;
    };
    this.$final = function () {
      this.FInputElement = undefined;
      this.FOnChange = undefined;
      pas.Controls.TWinControl.$final.call(this);
    };
    this.SetAlignment = function (AValue) {
      if (this.FAlignment !== AValue) {
        this.FAlignment = AValue;
        this.Changed();
      };
    };
    this.SetCharCase = function (AValue) {
      if (this.FCharCase !== AValue) {
        this.FCharCase = AValue;
        this.Changed();
      };
    };
    this.SetMaxLength = function (AValue) {
      if (this.FMaxLength !== AValue) {
        this.FMaxLength = AValue;
        this.Changed();
      };
    };
    this.SetPasswordChar = function (AValue) {
      if (this.FPasswordChar !== AValue) {
        this.FPasswordChar = AValue;
        this.Changed();
      };
    };
    this.SetReadOnly = function (AValue) {
      if (this.FReadOnly !== AValue) {
        this.FReadOnly = AValue;
        this.Changed();
      };
    };
    this.SetSelLength = function (AValue) {
      if (AValue < 0) {
        AValue = 0;
      };
      if (this.FSelLength !== AValue) {
        this.FSelLength = AValue;
        this.Changed();
      };
    };
    this.SetSelStart = function (AValue) {
      if (this.FSelStart !== AValue) {
        this.FSelStart = AValue;
        this.Changed();
      };
    };
    this.SetTextHint = function (AValue) {
      if (this.FTextHint !== AValue) {
        this.FTextHint = AValue;
        this.Changed();
      };
    };
    this.Change = function () {
      if (this.FOnChange != null) {
        this.FOnChange(this);
      };
    };
    this.DoEnter = function () {
      pas.Controls.TWinControl.DoEnter.call(this);
      this.SelectAll();
    };
    this.DoInput = function (ANewValue) {
      if (ANewValue !== this.RealGetText()) {
        this.FText = ANewValue;
        this.FModified = true;
        this.Change();
      };
    };
    this.HandleInput = function (AEvent) {
      var Result = false;
      AEvent.stopPropagation();
      this.DoInput(this.FHandleElement.value);
      Result = true;
      return Result;
    };
    this.Changed = function () {
      pas.Controls.TControl.Changed.call(this);
      if (!this.IsUpdating() && !(0 in this.FComponentState)) {
        var $with = this.FInputElement;
        $with.style.setProperty("width","100%");
        $with.style.setProperty("height","100%");
        $with.style.setProperty("border-style","none");
        $with.style.setProperty("box-sizing","border-box");
        var $tmp = this.FAlignment;
        if ($tmp === 1) {
          $with.style.setProperty("text-align","right")}
         else if ($tmp === 2) {
          $with.style.setProperty("text-align","center")}
         else {
          $with.style.removeProperty("text-align");
        };
        var $tmp1 = this.FCharCase;
        if ($tmp1 === 2) {
          $with.style.setProperty("text-transform","lowercase")}
         else if ($tmp1 === 1) {
          $with.style.setProperty("text-transform","uppercase")}
         else {
          $with.style.removeProperty("text-transform");
        };
        if (this.FMaxLength > 0) {
          $with.maxLength = this.FMaxLength;
        } else {
          $with.removeAttribute("maxlength");
        };
        if (this.FPattern !== "") {
          $with.pattern = this.FPattern;
        } else {
          $with.removeAttribute("pattern");
        };
        if (this.FTextHint !== "") {
          $with.placeholder = this.FTextHint;
        } else {
          $with.removeAttribute("placeholder");
        };
        $with.readOnly = this.FReadOnly;
        $with.required = this.FRequired;
        var $tmp2 = this.InputType();
        if (($tmp2 === "text") || ($tmp2 === "search") || ($tmp2 === "URL") || ($tmp2 === "tel") || ($tmp2 === "password")) {
          $with.setSelectionRange(this.FSelStart,this.FSelStart + this.FSelLength);
        };
        $with.type = this.InputType();
        $with.value = this.RealGetText();
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("div");
      return Result;
    };
    this.CreateInputElement = function () {
      var Result = null;
      Result = this.FHandleElement.appendChild(document.createElement("input"));
      return Result;
    };
    this.RegisterHandleEvents = function () {
      pas.Controls.TWinControl.RegisterHandleEvents.call(this);
      var $with = this.FHandleElement;
      $with.addEventListener("input",rtl.createSafeCallback(this,"HandleInput"));
    };
    this.UnRegisterHandleEvents = function () {
      pas.Controls.TWinControl.UnRegisterHandleEvents.call(this);
      var $with = this.FHandleElement;
      $with.removeEventListener("input",rtl.createSafeCallback(this,"HandleInput"));
    };
    this.CheckChildClassAllowed = function (AChildClass) {
      var Result = false;
      Result = pas.Controls.TWinControl.CheckChildClassAllowed.call(this,AChildClass);
      return Result;
    };
    this.RealGetText = function () {
      var Result = "";
      Result = this.FText;
      return Result;
    };
    this.RealSetText = function (AValue) {
      this.FText = AValue;
      this.FModified = false;
      this.Changed();
    };
    this.InputType = function () {
      var Result = "";
      Result = pas.Controls.IfThen$3(this.FPasswordChar !== "\x00","password","text");
      return Result;
    };
    this.GetControlClassDefaultSize = function () {
      var Result = pas.Types.TSize.$new();
      Result.cx = 80;
      Result.cy = 25;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.Controls.TControl.Create$1.call(this,AOwner);
      this.FInputElement = this.CreateInputElement();
      this.FMaxLength = 0;
      this.FModified = false;
      this.FPasswordChar = "\x00";
      this.FPattern = "";
      this.FReadOnly = false;
      this.FTextHint = "";
      this.FText = "";
      this.BeginUpdate();
      try {
        var $with = this.$class.GetControlClassDefaultSize();
        this.SetBounds(0,0,$with.cx,$with.cy);
      } finally {
        this.EndUpdate();
      };
      return this;
    };
    this.SelectAll = function () {
      if (this.RealGetText() !== "") {
        this.BeginUpdate();
        try {
          this.SetSelStart(0);
          this.SetSelLength(this.RealGetText().length);
        } finally {
          this.EndUpdate();
        };
      };
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  rtl.createClass(this,"TCustomMemo",pas.Controls.TWinControl,function () {
    this.$init = function () {
      pas.Controls.TWinControl.$init.call(this);
      this.FAlignment = 0;
      this.FCharCase = 0;
      this.FLines = null;
      this.FMaxLength = 0;
      this.FModified = false;
      this.FReadOnly = false;
      this.FTextAreaElement = null;
      this.FTextHint = "";
      this.FWantReturns = false;
      this.FWantTabs = false;
      this.FWordWrap = false;
      this.FOnChange = null;
    };
    this.$final = function () {
      this.FLines = undefined;
      this.FTextAreaElement = undefined;
      this.FOnChange = undefined;
      pas.Controls.TWinControl.$final.call(this);
    };
    this.HandleLinesChange = function (aSender) {
      this.Changed();
    };
    this.SetAlignment = function (AValue) {
      if (this.FAlignment !== AValue) {
        this.FAlignment = AValue;
        this.Changed();
      };
    };
    this.SetCharCase = function (AValue) {
      if (this.FCharCase !== AValue) {
        this.FCharCase = AValue;
        this.Changed();
      };
    };
    this.SetLines = function (AValue) {
      this.FLines.Assign(AValue);
      this.Changed();
    };
    this.SetMaxLength = function (AValue) {
      if (this.FMaxLength !== AValue) {
        this.FMaxLength = AValue;
        this.Changed();
      };
    };
    this.SetReadOnly = function (AValue) {
      if (this.FReadOnly !== AValue) {
        this.FReadOnly = AValue;
        this.Changed();
      };
    };
    this.SetTextHint = function (AValue) {
      if (this.FTextHint !== AValue) {
        this.FTextHint = AValue;
      };
    };
    this.SetWantReturns = function (AValue) {
      if (this.FWantReturns !== AValue) {
        this.FWantReturns = AValue;
      };
    };
    this.SetWantTabs = function (AValue) {
      if (this.FWantTabs !== AValue) {
        this.FWantTabs = AValue;
      };
    };
    this.SetWordWrap = function (AValue) {
      if (this.FWordWrap !== AValue) {
        this.FWordWrap = AValue;
        this.Changed();
      };
    };
    this.Change = function () {
      if (this.FOnChange != null) {
        this.FOnChange(this);
      };
    };
    this.KeyDown = function (Key, Shift) {
      var StartPos = 0;
      var NewText = "";
      pas.Controls.TWinControl.KeyDown.call(this,Key,rtl.refSet(Shift));
      if (!this.FWantReturns && (Key.get() === 13)) {
        Key.set(0);
      };
      if (this.FWantTabs && (Key.get() === 9)) {
        StartPos = this.FHandleElement.selectionStart;
        NewText = this.GetText();
        pas.System.Insert("\t",{get: function () {
            return NewText;
          }, set: function (v) {
            NewText = v;
          }},StartPos + 1);
        this.SetText(NewText);
        this.FHandleElement.selectionEnd = StartPos + 1;
        Key.set(0);
      };
    };
    this.HandleChange = function (AEvent) {
      var Result = false;
      var VNewText = "";
      var VOldText = "";
      AEvent.stopPropagation();
      VNewText = this.FHandleElement.value;
      VOldText = this.RealGetText();
      if (VNewText !== VOldText) {
        this.FLines.SetTextStr(VNewText);
        this.FModified = true;
        this.Change();
      };
      Result = true;
      return Result;
    };
    this.Changed = function () {
      pas.Controls.TControl.Changed.call(this);
      if (!this.IsUpdating() && !(0 in this.FComponentState)) {
        var $with = this.FTextAreaElement;
        $with.style.setProperty("border","none");
        var $tmp = this.FAlignment;
        if ($tmp === 1) {
          $with.style.setProperty("text-align","right")}
         else if ($tmp === 2) {
          $with.style.setProperty("text-align","center")}
         else {
          $with.style.removeProperty("text-align");
        };
        $with.style.setProperty("width","100%");
        $with.style.setProperty("height","100%");
        $with.style.setProperty("border-style","none");
        $with.style.setProperty("box-sizing","border-box");
        var $tmp1 = this.FCharCase;
        if ($tmp1 === 2) {
          $with.style.setProperty("text-transform","lowercase")}
         else if ($tmp1 === 1) {
          $with.style.setProperty("text-transform","uppercase")}
         else {
          $with.style.removeProperty("text-transform");
        };
        if (this.FMaxLength > 0) {
          $with.maxLength = this.FMaxLength;
        } else {
          $with.removeAttribute("maxlength");
        };
        if (this.FTextHint !== "") {
          $with.placeholder = this.FTextHint;
        } else {
          $with.removeAttribute("placeholder");
        };
        $with.readOnly = this.FReadOnly;
        $with.style.setProperty("resize","none");
        if (this.FWordWrap) {
          $with.removeAttribute("wrap");
        } else {
          $with.wrap = "off";
        };
        $with.style.setProperty("overflow","auto");
        $with.value = this.RealGetText();
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("div");
      return Result;
    };
    this.CreateTextAreElement = function () {
      var Result = null;
      Result = this.FHandleElement.appendChild(document.createElement("textarea"));
      return Result;
    };
    this.RegisterHandleEvents = function () {
      pas.Controls.TWinControl.RegisterHandleEvents.call(this);
      var $with = this.FHandleElement;
      $with.addEventListener("input",rtl.createSafeCallback(this,"HandleChange"));
    };
    this.UnRegisterHandleEvents = function () {
      pas.Controls.TWinControl.UnRegisterHandleEvents.call(this);
      var $with = this.FHandleElement;
      $with.removeEventListener("input",rtl.createSafeCallback(this,"HandleChange"));
    };
    this.CheckChildClassAllowed = function (AChildClass) {
      var Result = false;
      Result = false;
      return Result;
    };
    this.RealGetText = function () {
      var Result = "";
      Result = this.FLines.GetTextStr();
      return Result;
    };
    this.RealSetText = function (AValue) {
      this.FLines.SetTextStr(AValue);
      this.FModified = false;
      this.Changed();
    };
    this.GetControlClassDefaultSize = function () {
      var Result = pas.Types.TSize.$new();
      Result.cx = 150;
      Result.cy = 90;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.Controls.TControl.Create$1.call(this,AOwner);
      this.FTextAreaElement = this.CreateTextAreElement();
      this.FLines = $impl.TCustomMemoStrings.$create("Create$1");
      this.FLines.FOnChange = rtl.createCallback(this,"HandleLinesChange");
      this.FMaxLength = 0;
      this.FModified = false;
      this.FReadOnly = false;
      this.FTextHint = "";
      this.FWantReturns = true;
      this.FWantTabs = false;
      this.FWordWrap = true;
      this.BeginUpdate();
      try {
        var $with = this.$class.GetControlClassDefaultSize();
        this.SetBounds(0,0,$with.cx,$with.cy);
      } finally {
        this.EndUpdate();
      };
      return this;
    };
    this.Destroy = function () {
      this.FLines.$destroy("Destroy");
      this.FLines = null;
      pas.Controls.TControl.Destroy.call(this);
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  rtl.createClass(this,"TCustomButton",pas.Controls.TWinControl,function () {
    this.$init = function () {
      pas.Controls.TWinControl.$init.call(this);
      this.FModalResult = 0;
      this.FButtonElement = null;
    };
    this.$final = function () {
      this.FButtonElement = undefined;
      pas.Controls.TWinControl.$final.call(this);
    };
    this.Changed = function () {
      pas.Controls.TControl.Changed.call(this);
      if (!this.IsUpdating() && !(0 in this.FComponentState)) {
        var $with = this.FButtonElement;
        $with.style.setProperty("width","100%");
        $with.style.setProperty("height","100%");
        $with.style.setProperty("font-family",this.FHandleElement.style.getPropertyValue("font-family"));
        this.FHandleElement.style.removeProperty("font-family");
        $with.style.setProperty("font-size",this.FHandleElement.style.getPropertyValue("font-size"));
        this.FHandleElement.style.removeProperty("font-size");
        $with.style.setProperty("padding","0");
        $with.innerHTML = this.GetText();
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("div");
      return Result;
    };
    this.CreateButtonElement = function () {
      var Result = null;
      Result = this.FHandleElement.appendChild(document.createElement("button"));
      return Result;
    };
    this.CheckChildClassAllowed = function (AChildClass) {
      var Result = false;
      Result = false;
      return Result;
    };
    this.GetControlClassDefaultSize = function () {
      var Result = pas.Types.TSize.$new();
      Result.cx = 80;
      Result.cy = 25;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.Controls.TControl.Create$1.call(this,AOwner);
      this.FButtonElement = this.CreateButtonElement();
      this.FModalResult = 0;
      this.BeginUpdate();
      try {
        var $with = this.$class.GetControlClassDefaultSize();
        this.SetBounds(0,0,$with.cx,$with.cy);
      } finally {
        this.EndUpdate();
      };
      return this;
    };
    this.AdjustSize = function () {
      var VSize = pas.Types.TSize.$new();
      pas.Controls.TControl.AdjustSize.call(this);
      VSize.$assign(this.FFont.TextExtent(this.GetText()));
      this.SetBounds(this.FLeft,this.FTop,VSize.cx,VSize.cy);
    };
    this.Click = function () {
      var VParent = null;
      if (this.FModalResult !== 0) {
        VParent = this.FParent;
        while (VParent != null) {
          if (pas.Forms.TCustomForm.isPrototypeOf(VParent)) {
            VParent.SetModalResult(this.FModalResult);
            break;
          };
          VParent = VParent.FParent;
        };
      };
      pas.Controls.TControl.Click.call(this);
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  this.TCheckBoxState = {"0": "cbUnchecked", cbUnchecked: 0, "1": "cbChecked", cbChecked: 1};
  this.$rtti.$Enum("TCheckBoxState",{minvalue: 0, maxvalue: 1, ordtype: 1, enumtype: this.TCheckBoxState});
  this.$rtti.$Enum("TLeftRight",{minvalue: 0, maxvalue: 1, ordtype: 1, enumtype: this.TAlignment});
  rtl.createClass(this,"TCustomCheckbox",pas.Controls.TWinControl,function () {
    this.$init = function () {
      pas.Controls.TWinControl.$init.call(this);
      this.FAlignment = pas.Classes.TAlignment.taLeftJustify;
      this.FLabelElement = null;
      this.FMarkElement = null;
      this.FState = 0;
      this.FOnChange = null;
    };
    this.$final = function () {
      this.FLabelElement = undefined;
      this.FMarkElement = undefined;
      this.FOnChange = undefined;
      pas.Controls.TWinControl.$final.call(this);
    };
    this.GetChecked = function () {
      var Result = false;
      Result = this.GetState() === 1;
      return Result;
    };
    this.GetState = function () {
      var Result = 0;
      Result = this.FState;
      return Result;
    };
    this.SetAlignment = function (AValue) {
      if (this.FAlignment !== AValue) {
        this.FAlignment = AValue;
      };
    };
    this.SetChecked = function (AValue) {
      if (AValue) {
        this.SetState(1);
      } else {
        this.SetState(0);
      };
    };
    this.SetState = function (AValue) {
      if (this.FState !== AValue) {
        this.FState = AValue;
        this.Changed();
        this.DoOnChange();
      };
    };
    this.DoOnChange = function () {
      if (this.FOnChange != null) {
        this.FOnChange(this);
      };
    };
    this.HandleClick = function (AEvent) {
      var Result = false;
      this.SetChecked(this.FState !== 1);
      Result = pas.Controls.TControl.HandleClick.call(this,AEvent);
      return Result;
    };
    this.Changed = function () {
      pas.Controls.TControl.Changed.call(this);
      if (!this.IsUpdating() && !(0 in this.FComponentState)) {
        var $with = this.FHandleElement;
        $with.style.setProperty("white-space","nowrap");
        $with.style.setProperty("user-select","none");
        $with.style.setProperty("-moz-user-select","none");
        $with.style.setProperty("-ms-user-select","none");
        $with.style.setProperty("-khtml-user-select","none");
        $with.style.setProperty("-webkit-user-select","none");
        $with.style.setProperty("display","flex");
        $with.style.setProperty("align-items","center");
        if (this.FAutoSize) {
          $with.style.removeProperty("height");
          $with.style.removeProperty("width");
        };
        var $with1 = this.FMarkElement;
        $with1.checked = this.FState === 1;
        $with1.type = "checkbox";
        var $with2 = this.FLabelElement;
        $with2.innerHTML = this.GetText();
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("div");
      return Result;
    };
    this.CreateMarkElement = function () {
      var Result = null;
      Result = this.FHandleElement.appendChild(document.createElement("input"));
      return Result;
    };
    this.CreateLabelElement = function () {
      var Result = null;
      Result = this.FHandleElement.appendChild(document.createElement("span"));
      return Result;
    };
    this.CheckChildClassAllowed = function (AChildClass) {
      var Result = false;
      Result = false;
      return Result;
    };
    this.GetControlClassDefaultSize = function () {
      var Result = pas.Types.TSize.$new();
      Result.cx = 90;
      Result.cy = 23;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.Controls.TControl.Create$1.call(this,AOwner);
      this.FMarkElement = this.CreateMarkElement();
      this.FLabelElement = this.CreateLabelElement();
      this.FAlignment = 1;
      this.FState = 0;
      try {
        var $with = this.$class.GetControlClassDefaultSize();
        this.SetBounds(0,0,$with.cx,$with.cy);
      } finally {
        this.EndUpdate();
      };
      return this;
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  rtl.createClass(this,"TCustomRadioButton",pas.Controls.TWinControl,function () {
    this.$init = function () {
      pas.Controls.TWinControl.$init.call(this);
      this.fInput = null;
      this.fLabel = null;
      this.FOnChange = null;
    };
    this.$final = function () {
      this.fInput = undefined;
      this.fLabel = undefined;
      this.FOnChange = undefined;
      pas.Controls.TWinControl.$final.call(this);
    };
    this.ChangeHandler = function (Event) {
      var Result = false;
      if (this.FOnChange != null) this.FOnChange(this);
      return Result;
    };
    this.GetChecked = function () {
      var Result = false;
      Result = this.fInput.checked;
      return Result;
    };
    this.LabelClickHandler = function (aEvent) {
      var Result = false;
      if (!this.GetChecked()) this.SetChecked(true);
      return Result;
    };
    this.SetChecked = function (AValue) {
      if (AValue === this.fInput.checked) return;
      this.fInput.checked = AValue;
      if (this.FOnChange != null) this.FOnChange(this);
    };
    this.Changed = function () {
      pas.Controls.TControl.Changed.call(this);
      this.FHandleElement.style.setProperty("display","flex");
      this.FHandleElement.style.setProperty("align-items","center");
      this.FHandleElement.style.setProperty("white-space","nowrap");
      if (this.FAutoSize) {
        this.FHandleElement.style.removeProperty("height");
        this.FHandleElement.style.removeProperty("width");
      };
      this.fInput.type = "radio";
      this.fInput.id = this.FName;
      this.fInput.name = this.FParent.FName;
      this.fInput.value = this.GetText();
      this.fLabel.textContent = this.GetText();
      this.fLabel.onclick = rtl.createSafeCallback(this,"LabelClickHandler");
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("div");
      this.fInput = document.createElement("input");
      this.fLabel = document.createElement("label");
      Result.append(this.fInput);
      this.fInput.onselect = rtl.createSafeCallback(this,"ChangeHandler");
      this.fInput.addEventListener("change",rtl.createSafeCallback(this,"ChangeHandler"));
      Result.append(this.fLabel);
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.Controls.TControl.Create$1.call(this,AOwner);
      this.SetAutoSize(true);
      return this;
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  rtl.createClass(this,"TCustomLabel",pas.Controls.TWinControl,function () {
    this.$init = function () {
      pas.Controls.TWinControl.$init.call(this);
      this.FAlignment = 0;
      this.FContentElement = null;
      this.FFocusControl = null;
      this.FLayout = 0;
      this.FTransparent = false;
      this.FWordWrap = false;
    };
    this.$final = function () {
      this.FContentElement = undefined;
      this.FFocusControl = undefined;
      pas.Controls.TWinControl.$final.call(this);
    };
    this.SetAlignment = function (AValue) {
      if (this.FAlignment !== AValue) {
        this.FAlignment = AValue;
        this.Changed();
      };
    };
    this.SetLayout = function (AValue) {
      if (this.FLayout !== AValue) {
        this.FLayout = AValue;
        this.Changed();
      };
    };
    this.SetTransparent = function (AValue) {
      if (this.FTransparent !== AValue) {
        this.FTransparent = AValue;
        this.BeginUpdate();
        try {
          if (this.FTransparent) {
            this.SetColor(536870911);
          } else if (this.FColor === 536870911) {
            this.SetColor(-2147483647);
          };
        } finally {
          this.EndUpdate();
        };
      };
    };
    this.SetWordWrap = function (AValue) {
      if (this.FWordWrap !== AValue) {
        this.FWordWrap = AValue;
        this.Changed();
      };
    };
    this.DoEnter = function () {
      pas.Controls.TWinControl.DoEnter.call(this);
      if ((this.FFocusControl != null) && this.FFocusControl.CanSetFocus()) {
        this.FFocusControl.SetFocus();
      };
    };
    this.Changed = function () {
      pas.Controls.TControl.Changed.call(this);
      if (!this.IsUpdating() && !(0 in this.FComponentState)) {
        var $with = this.FHandleElement;
        if (this.FTransparent) {
          $with.style.setProperty("background-color","transparent");
        };
        $with.style.setProperty("outline","none");
        $with.style.setProperty("user-select","none");
        $with.style.setProperty("-moz-user-select","none");
        $with.style.setProperty("-ms-user-select","none");
        $with.style.setProperty("-khtml-user-select","none");
        $with.style.setProperty("-webkit-user-select","none");
        if (this.FAutoSize) {
          $with.style.removeProperty("height");
          $with.style.removeProperty("width");
        };
        var $tmp = this.FAlignment;
        if ($tmp === 2) {
          $with.style.setProperty("text-align","center")}
         else if ($tmp === 0) {
          $with.style.setProperty("text-align","left")}
         else if ($tmp === 1) $with.style.setProperty("text-align","right");
        var $with1 = this.FContentElement;
        $with1.innerHTML = "";
        $with1.style.setProperty("display","table-cell");
        var $tmp1 = this.FLayout;
        if ($tmp1 === 2) {
          $with1.style.setProperty("vertical-align","bottom")}
         else if ($tmp1 === 1) {
          $with1.style.setProperty("vertical-align","middle")}
         else if ($tmp1 === 0) $with1.style.setProperty("vertical-align","top");
        if (this.FWordWrap) {
          $with1.style.setProperty("word-wrap","break-word");
          $with1.style.removeProperty("white-space");
        } else {
          $with1.style.setProperty("white-space","nowrap");
          $with1.style.removeProperty("word-wrap");
        };
        $with1.style.setProperty("overflow","hidden");
        $with1.style.setProperty("text-overflow","ellipsis");
        $with1.innerHTML = this.GetText();
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("div");
      return Result;
    };
    this.CreateContentElement = function () {
      var Result = null;
      Result = this.FHandleElement.appendChild(document.createElement("label"));
      return Result;
    };
    this.CheckChildClassAllowed = function (AChildClass) {
      var Result = false;
      Result = false;
      return Result;
    };
    this.GetControlClassDefaultSize = function () {
      var Result = pas.Types.TSize.$new();
      Result.cx = 65;
      Result.cy = 17;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.Controls.TControl.Create$1.call(this,AOwner);
      this.FContentElement = this.CreateContentElement();
      this.FAlignment = 0;
      this.FFocusControl = null;
      this.FLayout = 0;
      this.FTransparent = true;
      this.FWordWrap = false;
      this.BeginUpdate();
      try {
        this.SetTabStop(false);
        this.SetAutoSize(true);
        var $with = this.$class.GetControlClassDefaultSize();
        this.SetBounds(0,0,$with.cx,$with.cy);
      } finally {
        this.EndUpdate();
      };
      return this;
    };
    this.AdjustSize = function () {
      pas.Controls.TControl.AdjustSize.call(this);
      this.Changed();
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  $mod.$implcode = function () {
    rtl.createClass($impl,"TCustomMemoStrings",pas.Classes.TStringList,function () {
      this.DoReadData = function (Reader) {
        Reader.ReadListBegin();
        this.BeginUpdate();
        try {
          this.Clear();
          while (!Reader.EndOfList()) this.Add(Reader.ReadString());
        } finally {
          this.EndUpdate();
        };
        Reader.ReadListEnd();
      };
      this.DoWriteData = function (Writer) {
        var i = 0;
        var lStringsNoWordWrap = null;
        lStringsNoWordWrap = pas.Classes.TStringList.$create("Create$1");
        try {
          lStringsNoWordWrap.SetTextStr(this.GetTextStr());
          Writer.WriteListBegin();
          for (var $l = 0, $end = lStringsNoWordWrap.GetCount() - 1; $l <= $end; $l++) {
            i = $l;
            Writer.WriteString(lStringsNoWordWrap.Get(i));
          };
          Writer.WriteListEnd();
        } finally {
          lStringsNoWordWrap = rtl.freeLoc(lStringsNoWordWrap);
        };
      };
      this.DefineProperties = function (Filer) {
        var HasData = false;
        HasData = this.GetCount() > 0;
        Filer.DefineProperty("Strings",rtl.createCallback(this,"DoReadData"),rtl.createCallback(this,"DoWriteData"),HasData);
      };
    });
  };
},["RTLConsts"]);
rtl.module("ExtCtrls",["System","JS","Classes","SysUtils","Types","Web","Graphics","Controls"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass(this,"TCustomImage",pas.Controls.TCustomControl,function () {
    this.$init = function () {
      pas.Controls.TCustomControl.$init.call(this);
      this.FCenter = false;
      this.FPicture = null;
      this.FProportional = false;
      this.FStretch = false;
      this.FOnPictureChanged = null;
      this.FStretchInEnabled = false;
      this.FStretchOutEnabled = false;
      this.FTransparent = false;
      this.FURL = "";
    };
    this.$final = function () {
      this.FPicture = undefined;
      this.FOnPictureChanged = undefined;
      pas.Controls.TCustomControl.$final.call(this);
    };
    this.SetCenter = function (AValue) {
      if (this.FCenter !== AValue) {
        this.FCenter = AValue;
        this.PictureChanged(this);
      };
    };
    this.SetProportional = function (AValue) {
      if (this.FProportional !== AValue) {
        this.FProportional = AValue;
        this.PictureChanged(this);
      };
    };
    this.SetStretch = function (AValue) {
      if (this.FStretch !== AValue) {
        this.FStretch = AValue;
        this.PictureChanged(this);
      };
    };
    this.SetStretchInEnabled = function (AValue) {
      if (this.FStretchInEnabled !== AValue) ;
      this.FStretchInEnabled = AValue;
      this.PictureChanged(this);
    };
    this.SetStretchOutEnabled = function (AValue) {
      if (this.FStretchOutEnabled !== AValue) {
        this.FStretchOutEnabled = AValue;
        this.PictureChanged(this);
      };
    };
    this.SetTransparent = function (AValue) {
      if (this.FTransparent === AValue) {
        this.FTransparent = AValue;
      };
    };
    this.SetURL = function (AValue) {
      if (this.FURL === AValue) return;
      this.FURL = AValue;
      this.PictureChanged(this);
    };
    this.Changed = function () {
      pas.Controls.TControl.Changed.call(this);
      if (!this.IsUpdating() && !(0 in this.FComponentState)) {
        var $with = this.FHandleElement;
        $with.style.setProperty("outline","none");
        $with.style.setProperty("background-image",pas.SysUtils.Format("url('%s')",pas.System.VarRecs(18,this.FURL)));
        $with.style.setProperty("background-repeat","no-repeat");
        if (this.FCenter) {
          $with.style.setProperty("background-position","center  center");
        } else {
          $with.style.removeProperty("background-position");
        };
        if (this.FProportional) {
          $with.style.setProperty("background-size","contain");
        } else if (this.FStretch) {
          if (this.FStretchInEnabled && this.FStretchOutEnabled) {
            $with.style.setProperty("background-size","100% 100%");
          } else if (this.FStretchInEnabled) {
            $with.style.setProperty("background-size","auto 100%");
          } else if (this.FStretchOutEnabled) {
            $with.style.setProperty("background-size","100% auto");
          };
        } else {
          $with.style.setProperty("background-size","auto");
        };
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("div");
      return Result;
    };
    this.CheckChildClassAllowed = function (AChildClass) {
      var Result = false;
      Result = false;
      return Result;
    };
    this.PictureChanged = function (Sender) {
      this.Changed();
      if (this.FOnPictureChanged != null) {
        this.FOnPictureChanged(this);
      };
    };
    this.GetControlClassDefaultSize = function () {
      var Result = pas.Types.TSize.$new();
      Result.cx = 90;
      Result.cy = 90;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.Controls.TControl.Create$1.call(this,AOwner);
      this.FPicture = pas.Graphics.TPicture.$create("Create$1");
      this.FPicture.FOnChange = rtl.createCallback(this,"PictureChanged");
      this.FCenter = false;
      this.FProportional = false;
      this.FStretch = false;
      this.FStretchOutEnabled = true;
      this.FStretchInEnabled = true;
      this.FTransparent = false;
      this.BeginUpdate();
      try {
        var $with = this.$class.GetControlClassDefaultSize();
        this.SetBounds(0,0,$with.cx,$with.cy);
      } finally {
        this.EndUpdate();
      };
      return this;
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  this.$rtti.$Int("TBevelWidth",{minvalue: 1, maxvalue: 2147483647, ordtype: 5});
  rtl.createClass(this,"TCustomPanel",pas.Controls.TCustomControl,function () {
    this.$init = function () {
      pas.Controls.TCustomControl.$init.call(this);
      this.FAlignment = 0;
      this.FBevelColor = 0;
      this.FBevelInner = 0;
      this.FBevelOuter = 0;
      this.FBevelWidth = 0;
      this.FLayout = 0;
      this.FWordWrap = false;
    };
    this.SetAlignment = function (AValue) {
      if (this.FAlignment !== AValue) {
        this.FAlignment = AValue;
        this.Changed();
      };
    };
    this.SetBevelColor = function (AValue) {
      if (this.FBevelColor !== AValue) {
        this.FBevelColor = AValue;
        this.Changed();
      };
    };
    this.SetBevelInner = function (AValue) {
      if (this.FBevelInner !== AValue) {
        this.FBevelInner = AValue;
        this.Changed();
      };
    };
    this.SetBevelOuter = function (AValue) {
      if (this.FBevelOuter !== AValue) {
        this.FBevelOuter = AValue;
        this.Changed();
      };
    };
    this.SetBevelWidth = function (AValue) {
      if (this.FBevelWidth !== AValue) {
        this.FBevelWidth = AValue;
        this.Changed();
      };
    };
    this.SetWordWrap = function (AValue) {
      if (this.FWordWrap !== AValue) {
        this.FWordWrap = AValue;
        this.Changed();
      };
    };
    this.Changed = function () {
      var VTopColor = 0;
      var VBottomColor = 0;
      pas.Controls.TControl.Changed.call(this);
      if (!this.IsUpdating() && !(0 in this.FComponentState)) {
        var $with = this.FHandleElement;
        if (this.FBevelOuter === 0) {
          $with.style.removeProperty("border-width");
          $with.style.removeProperty("border-left-color");
          $with.style.removeProperty("border-left-style");
          $with.style.removeProperty("border-top-color");
          $with.style.removeProperty("border-top-style");
          $with.style.removeProperty("border-right-color");
          $with.style.removeProperty("border-right-style");
          $with.style.removeProperty("border-bottom-color");
          $with.style.removeProperty("border-bottom-style");
        } else {
          if (this.FBevelColor === 536870912) {
            var $tmp = this.FBevelOuter;
            if ($tmp === 1) {
              VTopColor = 8421504;
              VBottomColor = 16777215;
            } else if ($tmp === 2) {
              VTopColor = 16777215;
              VBottomColor = 8421504;
            } else {
              VTopColor = this.FColor;
              VBottomColor = this.FColor;
            };
          } else {
            VTopColor = this.FBevelColor;
            VBottomColor = this.FBevelColor;
          };
          $with.style.setProperty("border-width",pas.SysUtils.IntToStr(this.FBevelWidth) + "px");
          $with.style.setProperty("border-style","solid");
          $with.style.setProperty("border-left-color",pas.Graphics.JSColor(VTopColor,this.FAlpha));
          $with.style.setProperty("border-top-color",pas.Graphics.JSColor(VTopColor,this.FAlpha));
          $with.style.setProperty("border-right-color",pas.Graphics.JSColor(VBottomColor,this.FAlpha));
          $with.style.setProperty("border-bottom-color",pas.Graphics.JSColor(VBottomColor,this.FAlpha));
        };
        $with.style.setProperty("outline","none");
        $with.style.setProperty("user-select","none");
        $with.style.setProperty("-moz-user-select","none");
        $with.style.setProperty("-ms-user-select","none");
        $with.style.setProperty("-khtml-user-select","none");
        $with.style.setProperty("-webkit-user-select","none");
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("div");
      return Result;
    };
    this.GetControlClassDefaultSize = function () {
      var Result = pas.Types.TSize.$new();
      Result.cx = 170;
      Result.cy = 50;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.Controls.TControl.Create$1.call(this,AOwner);
      this.FAlignment = 2;
      this.FBevelColor = 536870912;
      this.FBevelOuter = 2;
      this.FBevelInner = 0;
      this.FBevelWidth = 1;
      this.FLayout = 1;
      this.FWordWrap = false;
      this.BeginUpdate();
      try {
        this.SetTabStop(false);
        var $with = this.$class.GetControlClassDefaultSize();
        this.SetBounds(0,0,$with.cx,$with.cy);
      } finally {
        this.EndUpdate();
      };
      return this;
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  rtl.createClass(this,"TCustomTimer",pas.Classes.TComponent,function () {
    this.$init = function () {
      pas.Classes.TComponent.$init.call(this);
      this.FEnabled = false;
      this.FInterval = 0;
      this.FTimerHandle = 0;
      this.FOnStartTimer = null;
      this.FOnStopTimer = null;
      this.FOnTimer = null;
    };
    this.$final = function () {
      this.FOnStartTimer = undefined;
      this.FOnStopTimer = undefined;
      this.FOnTimer = undefined;
      pas.Classes.TComponent.$final.call(this);
    };
    this.SetEnabled = function (AValue) {
      if (this.FEnabled === AValue) return;
      this.FEnabled = AValue;
      this.UpdateTimer();
    };
    this.SetInterval = function (AValue) {
      if (this.FInterval === AValue) return;
      this.FInterval = AValue;
      this.UpdateTimer();
    };
    this.SetOnTimer = function (AValue) {
      if (rtl.eqCallback(this.FOnTimer,AValue)) return;
      this.FOnTimer = AValue;
      this.UpdateTimer();
    };
    this.UpdateTimer = function () {
      var $Self = this;
      this.KillTimer();
      if (this.FEnabled && (this.FInterval > 0) && rtl.eqSet(rtl.intersectSet(rtl.createSet(0,3),this.FComponentState),{}) && (this.FOnTimer != null)) {
        this.FTimerHandle = window.setInterval(function () {
          $Self.FOnTimer($Self);
        },this.FInterval);
        if (this.FTimerHandle === 0) throw pas.Classes.EOutOfResources.$create("Create$1",[rtl.getResStr(pas.WCLStrConsts,"rsNoTimers")]);
        if (this.FOnStartTimer != null) this.FOnStartTimer($Self);
      };
    };
    this.KillTimer = function () {
      if (this.FTimerHandle !== 0) {
        window.clearInterval(this.FTimerHandle);
        if (this.FOnStopTimer != null) this.FOnStopTimer(this);
      };
    };
    this.Loaded = function () {
      pas.Classes.TComponent.Loaded.call(this);
      this.UpdateTimer();
    };
    this.Create$1 = function (AOwner) {
      pas.Classes.TComponent.Create$1.call(this,AOwner);
      this.FEnabled = true;
      this.FInterval = 1000;
      this.FTimerHandle = 0;
      return this;
    };
    this.Destroy = function () {
      this.KillTimer();
      pas.Classes.TComponent.Destroy.call(this);
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  rtl.createClass(this,"TCustomSplitter",pas.Controls.TCustomControl,function () {
    this.$init = function () {
      pas.Controls.TCustomControl.$init.call(this);
      this.FMouseUpHandler = null;
      this.FMouseMoveHandler = null;
      this.FResizeControl = null;
    };
    this.$final = function () {
      this.FMouseUpHandler = undefined;
      this.FMouseMoveHandler = undefined;
      this.FResizeControl = undefined;
      pas.Controls.TCustomControl.$final.call(this);
    };
    this.DoMouseMove = function (aEvent) {
      var Result = false;
      if (!(this.FResizeControl != null)) return Result;
      var $tmp = this.FAlign;
      if ($tmp === 3) {
        this.FResizeControl.SetWidth(Math.round(aEvent.clientX))}
       else if ($tmp === 4) {
        this.FResizeControl.SetWidth(this.FResizeControl.FParent.FWidth - Math.round(aEvent.clientX))}
       else if ($tmp === 1) {
        this.FResizeControl.SetHeight(Math.round(aEvent.clientY))}
       else if ($tmp === 2) this.FResizeControl.SetHeight(this.FResizeControl.FParent.FHeight - Math.round(aEvent.clientY));
      Result = true;
      return Result;
    };
    this.DoMouseUp = function (aEvent) {
      var Result = false;
      document.removeEventListener("mousemove",this.FMouseMoveHandler);
      document.removeEventListener("mouseup",this.FMouseUpHandler);
      Result = true;
      return Result;
    };
    this.GetResizeControl = function () {
      var $Self = this;
      var Result = null;
      var i = 0;
      var CurControl = null;
      var BestValue = 0;
      function FindNearerControl(CurValue, Limit) {
        if ((CurValue <= Limit) && ((Result === null) || (BestValue < CurValue))) {
          BestValue = CurValue;
          Result = CurControl;
        };
      };
      Result = null;
      BestValue = 0;
      if (!(this.FAlign in rtl.createSet(3,1,4,2))) return Result;
      for (var $l = 0, $end = this.FParent.GetControlCount() - 1; $l <= $end; $l++) {
        i = $l;
        CurControl = this.FParent.GetControl(i);
        if ((CurControl !== $Self) && CurControl.FVisible && ((CurControl.FAlign === $Self.FAlign) || (CurControl.FAlign === 5))) {
          var $tmp = $Self.FAlign;
          if ($tmp === 3) {
            FindNearerControl(CurControl.FLeft + CurControl.FWidth,this.FLeft)}
           else if ($tmp === 1) {
            FindNearerControl(CurControl.FTop + CurControl.FHeight,this.FTop)}
           else if ($tmp === 4) {
            FindNearerControl(-CurControl.FLeft,-this.FLeft - this.FWidth)}
           else if ($tmp === 2) FindNearerControl(-CurControl.FTop,-this.FTop - this.FHeight);
        };
      };
      return Result;
    };
    this.Changed = function () {
      pas.Controls.TControl.Changed.call(this);
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("div");
      return Result;
    };
    this.MouseDown = function (Button, Shift, X, Y) {
      pas.Controls.TControl.MouseDown.call(this,Button,rtl.refSet(Shift),X,Y);
      if (Button === 0) {
        this.FMouseMoveHandler = rtl.createSafeCallback(this,"DoMouseMove");
        this.FMouseUpHandler = rtl.createSafeCallback(this,"DoMouseUp");
        document.addEventListener("mousemove",this.FMouseMoveHandler);
        document.addEventListener("mouseup",this.FMouseUpHandler);
        this.FResizeControl = this.GetResizeControl();
      };
    };
    this.Create$1 = function (AOwner) {
      pas.Controls.TControl.Create$1.call(this,AOwner);
      return this;
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
},["WCLStrConsts"]);
rtl.module("ComCtrls",["System","Classes","SysUtils","Types","JS","Web","Graphics","Controls"],function () {
  "use strict";
  var $mod = this;
  this.TTabPosition = {"0": "tpTop", tpTop: 0, "1": "tpBottom", tpBottom: 1, "2": "tpLeft", tpLeft: 2, "3": "tpRight", tpRight: 3};
  this.$rtti.$Enum("TTabPosition",{minvalue: 0, maxvalue: 3, ordtype: 1, enumtype: this.TTabPosition});
  rtl.createClass(this,"TCustomTabSheet",pas.Controls.TWinControl,function () {
    this.$init = function () {
      pas.Controls.TWinControl.$init.call(this);
      this.FTabVisible = false;
    };
    this.GetPageIndex = function () {
      var Result = 0;
      if ($mod.TCustomPageControl.isPrototypeOf(this.FParent)) {
        Result = this.FParent.IndexOf(this);
      } else {
        Result = -1;
      };
      return Result;
    };
    this.Changed = function () {
      pas.Controls.TControl.Changed.call(this);
      if (!this.IsUpdating() && !(0 in this.FComponentState)) {
        var $with = this.FHandleElement;
        $with.style.setProperty("background-color","#fff");
        $with.style.setProperty("outline","none");
        $with.style.setProperty("border","1px solid #c9c3ba");
        $with.style.setProperty("border-top","0px");
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("span");
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.Controls.TControl.Create$1.call(this,AOwner);
      this.FTabVisible = true;
      this.BeginUpdate();
      try {
        this.SetVisible(false);
      } finally {
        this.EndUpdate();
      };
      return this;
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
  rtl.createClass(this,"TTabSheet",this.TCustomTabSheet,function () {
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Caption",3,pas.Controls.$rtti["TCaption"],"GetText","SetText");
    $r.addProperty("ClientHeight",3,rtl.nativeint,"GetClientHeight","SetClientHeight");
    $r.addProperty("ClientWidth",3,rtl.nativeint,"GetClientWidth","SetClientWidth");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("Height",2,rtl.nativeint,"FHeight","SetHeight");
    $r.addProperty("Left",2,rtl.nativeint,"FLeft","SetLeft");
    $r.addProperty("PageIndex",1,rtl.nativeint,"GetPageIndex","");
    $r.addProperty("ParentFont",2,rtl.boolean,"FParentFont","SetParentFont");
    $r.addProperty("ParentShowHint",2,rtl.boolean,"FParentShowHint","SetParentShowHint");
    $r.addProperty("PopupMenu",2,pas.Controls.$rtti["TCustomPopupMenu"],"FPopupMenu","SetPopupMenu");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("TabVisible",0,rtl.boolean,"FTabVisible","FTabVisible");
    $r.addProperty("Top",2,rtl.nativeint,"FTop","SetTop");
    $r.addProperty("Width",2,rtl.nativeint,"FWidth","SetWidth");
    $r.addProperty("OnEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,pas.Classes.$rtti["TNotifyEvent"],"FOnExit","FOnExit");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
  });
  rtl.createClass(this,"TCustomPageControl",pas.Controls.TWinControl,function () {
    this.$init = function () {
      pas.Controls.TWinControl.$init.call(this);
      this.FPageIndex = 0;
      this.FPages = null;
      this.FShowTabs = false;
      this.FTabContainerElement = null;
      this.FTabHeight = 0;
      this.FTabPosition = 0;
      this.FTabWidth = 0;
    };
    this.$final = function () {
      this.FPages = undefined;
      this.FTabContainerElement = undefined;
      pas.Controls.TWinControl.$final.call(this);
    };
    this.GetActivePage = function () {
      var Result = null;
      Result = this.GetPage(this.FPageIndex);
      return Result;
    };
    this.GetPage = function (AIndex) {
      var Result = null;
      if ((AIndex >= 0) && (AIndex < this.FPages.length)) {
        Result = rtl.getObject(this.FPages[AIndex]);
      } else {
        Result = null;
      };
      return Result;
    };
    this.SetActivePage = function (AValue) {
      this.SetPageIndex(this.FPages.indexOf(AValue));
    };
    this.SetPageIndex = function (AValue) {
      if ((AValue < 0) || (AValue >= this.FPages.length)) {
        AValue = 0;
      };
      if (AValue !== this.FPageIndex) {
        this.FPageIndex = AValue;
        this.Changed();
      };
    };
    this.SetShowTabs = function (AValue) {
      if (this.FShowTabs !== AValue) {
        this.FShowTabs = AValue;
        this.Changed();
      };
    };
    this.SetTabHeight = function (AValue) {
      if (this.FTabHeight !== AValue) {
        this.FTabHeight = AValue;
        this.Changed();
      };
    };
    this.SetTabPosition = function (AValue) {
      if (this.FTabPosition !== AValue) {
        this.FTabPosition = AValue;
      };
    };
    this.SetTabWidth = function (AValue) {
      if (this.FTabWidth !== AValue) {
        this.FTabWidth = AValue;
        this.Changed();
      };
    };
    this.Changed = function () {
      pas.Controls.TControl.Changed.call(this);
      if (!this.IsUpdating() && !(0 in this.FComponentState)) {
        var $with = this.FHandleElement;
        $with.style.setProperty("outline","none");
        this.RenderTabs();
        this.UpdatePages();
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("div");
      return Result;
    };
    this.CreateTabContainerElement = function () {
      var Result = null;
      Result = document.createElement("span");
      this.FHandleElement.appendChild(Result);
      return Result;
    };
    this.RegisterChild = function (AControl) {
      var VIndex = 0;
      pas.Controls.TControl.RegisterChild.call(this,AControl);
      if ((AControl != null) && $mod.TCustomTabSheet.isPrototypeOf(AControl)) {
        VIndex = this.FPages.indexOf(AControl);
        if (VIndex < 0) {
          this.FPages.push(AControl);
        };
      };
    };
    this.UnRegisterChild = function (AControl) {
      var VIndex = 0;
      pas.Controls.TControl.UnRegisterChild.call(this,AControl);
      if ((AControl != null) && $mod.TCustomTabSheet.isPrototypeOf(AControl)) {
        VIndex = this.FPages.indexOf(AControl);
        if (VIndex >= 0) {
          this.FPages.splice(VIndex,1);
        };
      };
    };
    this.CalcTabHeight = function () {
      var Result = 0;
      if (this.FShowTabs) {
        if (this.FTabHeight > 0) {
          Result = this.FTabHeight;
        } else {
          Result = this.FFont.TextHeight("Fj") + 10;
        };
      } else {
        Result = 0;
      };
      return Result;
    };
    this.CalcTabWidth = function (AText) {
      var Result = 0;
      if (this.FTabWidth > 0) {
        Result = this.FTabWidth;
      } else {
        Result = this.FFont.TextWidth(AText) + 10;
      };
      return Result;
    };
    this.CalcMaxTabWidth = function () {
      var Result = 0;
      var VPage = null;
      var VIndex = 0;
      var VWidth = 0;
      Result = 0;
      if (this.FTabWidth > 0) {
        Result = this.FTabWidth;
      } else {
        for (var $l = 0, $end = this.FPages.length - 1; $l <= $end; $l++) {
          VIndex = $l;
          VPage = rtl.getObject(this.FPages[VIndex]);
          if ((VPage != null) && VPage.FTabVisible) {
            VWidth = this.CalcTabWidth(VPage.GetText());
            if (VWidth > Result) {
              Result = VWidth;
            };
          };
        };
      };
      return Result;
    };
    this.CalcSumTabsWidth = function () {
      var Result = 0;
      var VIndex = 0;
      var VPage = null;
      Result = 0;
      for (var $l = 0, $end = this.FPages.length - 1; $l <= $end; $l++) {
        VIndex = $l;
        VPage = rtl.getObject(this.FPages[VIndex]);
        if ((VPage != null) && VPage.FTabVisible) {
          Result = Result + this.CalcTabWidth(VPage.GetText());
        };
      };
      return Result;
    };
    this.IndexOfTab = function (ACaption) {
      var Result = 0;
      var VIndex = 0;
      var VPage = null;
      Result = -1;
      for (var $l = 0, $end = this.FPages.length - 1; $l <= $end; $l++) {
        VIndex = $l;
        VPage = rtl.getObject(this.FPages[VIndex]);
        if ((VPage != null) && VPage.FTabVisible && pas.SysUtils.SameText(VPage.GetText(),ACaption)) {
          Result = VIndex;
        };
      };
      return Result;
    };
    this.RenderTab = function (ACaption, ALeft, ATop, AWidth, AHeight, AEvent) {
      var Result = null;
      Result = document.createElement("button");
      Result.style.setProperty("left",pas.SysUtils.IntToStr(ALeft) + "px");
      Result.style.setProperty("top",pas.SysUtils.IntToStr(ATop) + "px");
      Result.style.setProperty("width",pas.SysUtils.IntToStr(AWidth) + "px");
      Result.style.setProperty("height",pas.SysUtils.IntToStr(AHeight) + "px");
      Result.style.setProperty("border","1px solid #c9c3ba");
      Result.style.setProperty("border-top-left-radius","15px");
      Result.style.setProperty("border-top-right-radius","2px");
      Result.style.setProperty("background-color","#dddada");
      Result.style.setProperty("color",pas.Graphics.JSColor(this.FFont.FColor,this.FAlpha));
      Result.style.setProperty("font",pas.Graphics.JSFont(this.FFont));
      Result.style.setProperty("outline","none");
      Result.style.setProperty("position","absolute");
      Result.style.setProperty("overflow","hidden");
      Result.style.setProperty("padding","0");
      Result.style.setProperty("white-space","nowrap");
      Result.addEventListener("click",AEvent);
      Result.innerHTML = ACaption;
      return Result;
    };
    this.RenderTabActive = function (ACaption, ALeft, ATop, AWidth, AHeight, AEvent) {
      var Result = null;
      Result = this.RenderTab(ACaption,ALeft,ATop,AWidth,AHeight,AEvent);
      Result.style.setProperty("border-bottom","0px");
      Result.style.setProperty("background-color","#fff");
      return Result;
    };
    this.RenderTabLeft = function (ALeft, ATop, AWidth, AHeight, AEvent) {
      var Result = null;
      Result = this.RenderTab("‹",ALeft,ATop,AWidth,AHeight,AEvent);
      Result.style.setProperty("background-color","#fff");
      return Result;
    };
    this.RenderTabRight = function (ALeft, ATop, AWidth, AHeight, AEvent) {
      var Result = null;
      Result = this.RenderTab("›",ALeft,ATop,AWidth,AHeight,AEvent);
      Result.style.setProperty("background-color","#fff");
      return Result;
    };
    this.RenderTabs = function () {
      var $Self = this;
      var form = null;
      function AdjustWithPPI(aValue) {
        var Result = 0;
        if (form != null) {
          Result = pas.System.Trunc((96 * aValue) / form.FDesignTimePPI)}
         else Result = aValue;
        return Result;
      };
      function FindParentForm() {
        var Result = null;
        var p = null;
        p = $Self.FParent;
        while ((p != null) && !pas.Forms.TCustomForm.isPrototypeOf(p)) p = p.FParent;
        if (pas.Forms.TCustomForm.isPrototypeOf(p)) {
          Result = p}
         else Result = null;
        return Result;
      };
      var VPage = null;
      var VIndex = 0;
      var VStartIndex = 0;
      var VEndIndex = 0;
      var VTabCaption = "";
      var VTabHeight = 0;
      var VTabLeft = 0;
      var VTabWidth = 0;
      var VSumTabsWidth = 0;
      var VMaxTabWidth = 0;
      var VTabsCount = 0;
      form = FindParentForm();
      VTabHeight = this.CalcTabHeight();
      VSumTabsWidth = this.CalcSumTabsWidth();
      var $with = this.FTabContainerElement;
      $with.innerHTML = "";
      $with.style.setProperty("left","0px");
      $with.style.setProperty("top","0px");
      $with.style.setProperty("width",pas.SysUtils.IntToStr(AdjustWithPPI(pas.Controls.IfThen$2(VSumTabsWidth > this.FWidth,VSumTabsWidth,this.FWidth))) + "px");
      $with.style.setProperty("height",pas.SysUtils.IntToStr(AdjustWithPPI(VTabHeight)) + "px");
      $with.style.setProperty("position","absolute");
      $with.style.setProperty("overflow","hidden");
      if ((this.FPageIndex > -1) && (this.FPageIndex < this.FPages.length)) {
        if (VSumTabsWidth > this.FWidth) {
          VTabLeft = 40;
          VMaxTabWidth = this.CalcMaxTabWidth();
          VTabsCount = pas.System.Trunc(rtl.trunc((this.FWidth - 80) / VMaxTabWidth));
          if (VTabsCount === 0) {
            VTabsCount = 1;
          };
          if ((this.FPageIndex - VTabsCount) >= 0) {
            VStartIndex = (this.FPageIndex - VTabsCount) + 1;
            VEndIndex = this.FPageIndex;
          } else {
            VStartIndex = 0;
            VEndIndex = VTabsCount - 1;
          };
          VMaxTabWidth = pas.System.Trunc(rtl.trunc((this.FWidth - 80) / VTabsCount));
          for (var $l = VStartIndex, $end = VEndIndex; $l <= $end; $l++) {
            VIndex = $l;
            VPage = rtl.getObject(this.FPages[VIndex]);
            if ((VPage != null) && VPage.FTabVisible) {
              VTabCaption = VPage.GetText();
              VTabWidth = VMaxTabWidth;
              if (VIndex === this.FPageIndex) {
                this.FTabContainerElement.appendChild(this.RenderTabActive(VTabCaption,AdjustWithPPI(VTabLeft),0,AdjustWithPPI(VTabWidth),AdjustWithPPI(VTabHeight),rtl.createCallback($Self,"TabClick")));
              } else {
                this.FTabContainerElement.appendChild(this.RenderTab(VTabCaption,AdjustWithPPI(VTabLeft),0,AdjustWithPPI(VTabWidth),AdjustWithPPI(VTabHeight),rtl.createCallback($Self,"TabClick")));
              };
              VTabLeft = VTabLeft + VTabWidth;
            };
          };
          var $with1 = this.FTabContainerElement;
          $with1.appendChild(this.RenderTabLeft(0,0,40,AdjustWithPPI(VTabHeight),rtl.createCallback($Self,"TabLeftClick")));
          $with1.appendChild(this.RenderTabRight(AdjustWithPPI(this.FWidth - 40),0,40,AdjustWithPPI(VTabHeight),rtl.createCallback($Self,"TabRightClick")));
        } else {
          VTabLeft = 0;
          VStartIndex = 0;
          VEndIndex = this.FPages.length - 1;
          VTabWidth = rtl.trunc(this.FWidth / this.FPages.length);
          for (var $l1 = VStartIndex, $end1 = VEndIndex; $l1 <= $end1; $l1++) {
            VIndex = $l1;
            VPage = rtl.getObject(this.FPages[VIndex]);
            if ((VPage != null) && VPage.FTabVisible) {
              VTabCaption = VPage.GetText();
              if (VIndex === this.FPageIndex) {
                this.FTabContainerElement.appendChild(this.RenderTabActive(VTabCaption,AdjustWithPPI(VTabLeft),0,AdjustWithPPI(VTabWidth),AdjustWithPPI(VTabHeight),rtl.createCallback($Self,"TabClick")));
              } else {
                this.FTabContainerElement.appendChild(this.RenderTab(VTabCaption,AdjustWithPPI(VTabLeft),0,AdjustWithPPI(VTabWidth),AdjustWithPPI(VTabHeight),rtl.createCallback($Self,"TabClick")));
              };
              VTabLeft = VTabLeft + VTabWidth;
            };
          };
        };
      };
    };
    this.TabClick = function (AEvent) {
      this.SetPageIndex(this.IndexOfTab(AEvent.target.innerHTML));
    };
    this.TabLeftClick = function (AEvent) {
      this.SetPageIndex(this.FPageIndex - 1);
    };
    this.TabRightClick = function (AEvent) {
      this.SetPageIndex(this.FPageIndex + 1);
    };
    this.UpdatePages = function () {
      var VIndex = 0;
      var VPage = null;
      var VTabHeight = 0;
      VTabHeight = this.CalcTabHeight();
      for (var $l = 0, $end = this.FPages.length - 1; $l <= $end; $l++) {
        VIndex = $l;
        VPage = rtl.getObject(this.FPages[VIndex]);
        if ((VPage != null) && VPage.FTabVisible) {
          VPage.BeginUpdate();
          try {
            if (VIndex === this.FPageIndex) {
              VPage.SetBounds(0,VTabHeight,this.FWidth,this.FHeight - VTabHeight);
              VPage.SetVisible(true);
            } else {
              VPage.SetVisible(false);
            };
          } finally {
            VPage.EndUpdate();
          };
        };
      };
    };
    this.GetControlClassDefaultSize = function () {
      var Result = pas.Types.TSize.$new();
      Result.cx = 200;
      Result.cy = 200;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.Controls.TControl.Create$1.call(this,AOwner);
      this.FTabContainerElement = this.CreateTabContainerElement();
      this.FPages = new Array();
      this.FPageIndex = -1;
      this.FShowTabs = true;
      this.FTabPosition = 0;
      this.BeginUpdate();
      try {
        this.SetTabStop(false);
        var $with = this.$class.GetControlClassDefaultSize();
        this.SetBounds(0,0,$with.cx,$with.cy);
      } finally {
        this.EndUpdate();
      };
      return this;
    };
    this.Destroy = function () {
      this.FPages.length = 0;
      pas.Controls.TControl.Destroy.call(this);
    };
    this.IndexOf = function (APage) {
      var Result = 0;
      Result = this.FPages.indexOf(APage);
      return Result;
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
},["Forms"]);
rtl.module("NumCtrls",["System","Classes","SysUtils","Types","Graphics","Controls","StdCtrls","Web"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass(this,"TCustomNumericEdit",pas.StdCtrls.TCustomEdit,function () {
    this.$init = function () {
      pas.StdCtrls.TCustomEdit.$init.call(this);
      this.FDecimals = 0;
    };
    this.DoEnter = function () {
      pas.StdCtrls.TCustomEdit.DoEnter.call(this);
      this.RealSetText(this.RealGetText());
    };
    this.DoExit = function () {
      pas.Controls.TWinControl.DoExit.call(this);
      this.RealSetText(this.RealGetText());
    };
    this.DoInput = function (ANewValue) {
      var VDiff = "";
      var VOldValue = "";
      VOldValue = this.RealGetText();
      if (ANewValue.length >= VOldValue.length) {
        VDiff = pas.SysUtils.StringReplace(ANewValue,VOldValue,"",{});
        if (VDiff === pas.SysUtils.DecimalSeparator) {
          if (this.FDecimals === 0) {
            VDiff = "";
          };
          if (pas.System.Pos(VDiff,VOldValue) > 0) {
            VDiff = "";
          };
        };
        if (!(VDiff.charCodeAt(0) in rtl.createSet(null,48,57,pas.SysUtils.DecimalSeparator.charCodeAt()))) {
          this.FHandleElement.value = VOldValue;
          ANewValue = VOldValue;
        };
      };
      pas.StdCtrls.TCustomEdit.DoInput.call(this,ANewValue);
    };
    this.Changed = function () {
      pas.StdCtrls.TCustomEdit.Changed.call(this);
      if (!this.IsUpdating() && !(0 in this.FComponentState)) {
        var $with = this.FHandleElement;
        $with.inputMode = "numeric";
      };
    };
    this.Create$1 = function (AOwner) {
      pas.StdCtrls.TCustomEdit.Create$1.call(this,AOwner);
      this.FDecimals = 2;
      this.BeginUpdate();
      try {
        this.SetAlignment(1);
      } finally {
        this.EndUpdate();
      };
      return this;
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
});
rtl.module("DttCtrls",["System","Classes","SysUtils","Types","Graphics","StdCtrls"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass(this,"TCustomDateTimeEdit",pas.StdCtrls.TCustomEdit,function () {
    this.DoEnter = function () {
      pas.StdCtrls.TCustomEdit.DoEnter.call(this);
      this.RealSetText(this.RealGetText());
    };
    this.DoExit = function () {
      pas.Controls.TWinControl.DoExit.call(this);
      this.RealSetText(this.RealGetText());
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
});
rtl.module("BtnCtrls",["System","Classes","SysUtils","Types","Web","Graphics","Controls","StdCtrls"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass(this,"TCustomFileButton",pas.Controls.TWinControl,function () {
    this.$init = function () {
      pas.Controls.TWinControl.$init.call(this);
      this.FFileSelect = null;
      this.FFilter = "";
      this.FOnChange = null;
      this.FOpendDialogElement = null;
    };
    this.$final = function () {
      this.FFileSelect = undefined;
      this.FOnChange = undefined;
      this.FOpendDialogElement = undefined;
      pas.Controls.TWinControl.$final.call(this);
    };
    this.SetFilter = function (AValue) {
      if (this.FFilter !== AValue) {
        this.FFilter = AValue;
        this.Changed();
      };
    };
    this.Change = function () {
      if (this.FOnChange != null) {
        this.FOnChange(this);
      };
    };
    this.HandleClick = function (AEvent) {
      var Result = false;
      Result = pas.Controls.TControl.HandleClick.call(this,AEvent);
      if (this.FOpendDialogElement != null) {
        this.FOpendDialogElement.click();
      };
      return Result;
    };
    this.HandleChange = function (AEvent) {
      var Result = false;
      var VFile = null;
      var VList = null;
      if (rtl.isExt(AEvent.target,HTMLInputElement)) {
        VList = AEvent.target.files;
        if (VList.length === 0) {
          this.FFileSelect = null;
          this.SetText(rtl.getResStr(pas.WCLStrConsts,"rsFileButtonNoFileSelected"));
          this.Changed();
          return false;
        };
        VFile = VList.item(0);
        this.FFileSelect = VFile;
        this.SetText(VFile.name);
        this.SetHint(VFile.name);
        this.Changed();
        this.Change();
        Result = true;
      };
      return Result;
    };
    this.Changed = function () {
      pas.Controls.TControl.Changed.call(this);
      if (!this.IsUpdating() && !(0 in this.FComponentState)) {
        var $with = this.FHandleElement;
        $with.style.setProperty("padding","0");
        $with.innerHTML = this.GetText();
        if (this.FOpendDialogElement != null) {
          var $with1 = this.FOpendDialogElement;
          $with1.accept = this.FFilter;
          $with1.type = "file";
        };
      };
    };
    this.CreateHandleElement = function () {
      var Result = null;
      Result = document.createElement("button");
      return Result;
    };
    this.CreateOpendDialogElement = function () {
      var Result = null;
      Result = this.FHandleElement.appendChild(document.createElement("input"));
      return Result;
    };
    this.CheckChildClassAllowed = function (AChildClass) {
      var Result = false;
      Result = false;
      return Result;
    };
    this.GetControlClassDefaultSize = function () {
      var Result = pas.Types.TSize.$new();
      Result.cx = 80;
      Result.cy = 25;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      pas.Controls.TControl.Create$1.call(this,AOwner);
      this.FOpendDialogElement = this.CreateOpendDialogElement();
      this.FOpendDialogElement.addEventListener("change",rtl.createSafeCallback(this,"HandleChange"));
      this.FFilter = "";
      this.FFileSelect = null;
      this.BeginUpdate();
      try {
        this.SetText(rtl.getResStr(pas.WCLStrConsts,"rsFileButtonNoFileSelected"));
        this.SetHint(this.GetText());
        var $with = this.$class.GetControlClassDefaultSize();
        this.SetBounds(0,0,$with.cx,$with.cy);
      } finally {
        this.EndUpdate();
      };
      return this;
    };
    this.Destroy = function () {
      if (this.FOpendDialogElement != null) {
        this.FOpendDialogElement.removeEventListener("change",rtl.createSafeCallback(this,"HandleChange"));
      };
      pas.Controls.TControl.Destroy.call(this);
    };
    this.AdjustSize = function () {
      var VSize = pas.Types.TSize.$new();
      pas.Controls.TControl.AdjustSize.call(this);
      VSize.$assign(this.FFont.TextExtent(this.GetText()));
      this.SetBounds(this.FLeft,this.FTop,VSize.cx,VSize.cy);
    };
    rtl.addIntf(this,pas.System.IUnknown);
  });
},["WCLStrConsts"]);
rtl.module("WebCtrls",["System","Classes","SysUtils","Types","Graphics","Controls","Forms","StdCtrls","ExtCtrls","ComCtrls","NumCtrls","DttCtrls","BtnCtrls"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass(this,"TWComboBox",pas.StdCtrls.TCustomComboBox,function () {
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("Anchors",14,pas.Controls.$rtti["TAnchors"],"FAnchors","SetAnchors",{stored: "IsAnchorsStored", Default: rtl.createSet(1,0)});
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize",{Default: false});
    $r.addProperty("BorderSpacing",2,pas.Controls.$rtti["TControlBorderSpacing"],"FBorderSpacing","SetBorderSpacing");
    $r.addProperty("BorderStyle",2,pas.Controls.$rtti["TBorderStyle"],"FBorderStyle","SetBorderStyle");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("ItemHeight",2,rtl.nativeint,"FItemHeight","SetItemHeight");
    $r.addProperty("ItemIndex",2,rtl.nativeint,"FItemIndex","SetItemIndex");
    $r.addProperty("Items",2,pas.Classes.$rtti["TStrings"],"FItems","SetItems");
    $r.addProperty("ParentColor",2,rtl.boolean,"FParentColor","SetParentColor");
    $r.addProperty("ParentFont",2,rtl.boolean,"FParentFont","SetParentFont");
    $r.addProperty("ParentShowHint",2,rtl.boolean,"FParentShowHint","SetParentShowHint");
    $r.addProperty("PopupMenu",2,pas.Controls.$rtti["TCustomPopupMenu"],"FPopupMenu","SetPopupMenu");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("TabOrder",2,rtl.nativeint,"FTabOrder","SetTabOrder");
    $r.addProperty("TabStop",2,rtl.boolean,"FTabStop","SetTabStop");
    $r.addProperty("Text",3,pas.Controls.$rtti["TCaption"],"GetText","SetText");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,pas.Classes.$rtti["TNotifyEvent"],"FOnExit","FOnExit");
    $r.addProperty("OnKeyDown",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyDown","FOnKeyDown");
    $r.addProperty("OnKeyPress",0,pas.Controls.$rtti["TKeyPressEvent"],"FOnKeyPress","FOnKeyPress");
    $r.addProperty("OnKeyUp",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
  });
  rtl.createClass(this,"TWListBox",pas.StdCtrls.TCustomListBox,function () {
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("Anchors",14,pas.Controls.$rtti["TAnchors"],"FAnchors","SetAnchors",{stored: "IsAnchorsStored", Default: rtl.createSet(1,0)});
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize",{Default: false});
    $r.addProperty("BorderSpacing",2,pas.Controls.$rtti["TControlBorderSpacing"],"FBorderSpacing","SetBorderSpacing");
    $r.addProperty("BorderStyle",2,pas.Controls.$rtti["TBorderStyle"],"FBorderStyle","SetBorderStyle");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("ItemHeight",2,rtl.nativeint,"FItemHeight","SetItemHeight");
    $r.addProperty("ItemIndex",2,rtl.nativeint,"FItemIndex","SetItemIndex",{Default: -1});
    $r.addProperty("Items",2,pas.Classes.$rtti["TStrings"],"FItems","SetItems");
    $r.addProperty("MultiSelect",2,rtl.boolean,"FMultiSelect","SetMultiSelect",{Default: false});
    $r.addProperty("ParentColor",2,rtl.boolean,"FParentColor","SetParentColor");
    $r.addProperty("ParentFont",2,rtl.boolean,"FParentFont","SetParentFont");
    $r.addProperty("ParentShowHint",2,rtl.boolean,"FParentShowHint","SetParentShowHint");
    $r.addProperty("PopupMenu",2,pas.Controls.$rtti["TCustomPopupMenu"],"FPopupMenu","SetPopupMenu");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("TabOrder",2,rtl.nativeint,"FTabOrder","SetTabOrder");
    $r.addProperty("TabStop",2,rtl.boolean,"FTabStop","SetTabStop");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,pas.Classes.$rtti["TNotifyEvent"],"FOnExit","FOnExit");
    $r.addProperty("OnKeyDown",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyDown","FOnKeyDown");
    $r.addProperty("OnKeyPress",0,pas.Controls.$rtti["TKeyPressEvent"],"FOnKeyPress","FOnKeyPress");
    $r.addProperty("OnKeyUp",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
    $r.addProperty("OnSelectionChange",0,pas.StdCtrls.$rtti["TSelectionChangeEvent"],"FOnSelectionChange","FOnSelectionChange");
  });
  rtl.createClass(this,"TWEdit",pas.StdCtrls.TCustomEdit,function () {
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("Anchors",14,pas.Controls.$rtti["TAnchors"],"FAnchors","SetAnchors",{stored: "IsAnchorsStored", Default: rtl.createSet(1,0)});
    $r.addProperty("Alignment",2,pas.Classes.$rtti["TAlignment"],"FAlignment","SetAlignment");
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize",{Default: false});
    $r.addProperty("BorderSpacing",2,pas.Controls.$rtti["TControlBorderSpacing"],"FBorderSpacing","SetBorderSpacing");
    $r.addProperty("BorderStyle",2,pas.Controls.$rtti["TBorderStyle"],"FBorderStyle","SetBorderStyle");
    $r.addProperty("CharCase",2,pas.StdCtrls.$rtti["TEditCharCase"],"FCharCase","SetCharCase");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("MaxLength",2,rtl.nativeint,"FMaxLength","SetMaxLength");
    $r.addProperty("ParentColor",2,rtl.boolean,"FParentColor","SetParentColor");
    $r.addProperty("ParentFont",2,rtl.boolean,"FParentFont","SetParentFont");
    $r.addProperty("ParentShowHint",2,rtl.boolean,"FParentShowHint","SetParentShowHint");
    $r.addProperty("PasswordChar",2,rtl.char,"FPasswordChar","SetPasswordChar");
    $r.addProperty("PopupMenu",2,pas.Controls.$rtti["TCustomPopupMenu"],"FPopupMenu","SetPopupMenu");
    $r.addProperty("ReadOnly",2,rtl.boolean,"FReadOnly","SetReadOnly");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("TabStop",2,rtl.boolean,"FTabStop","SetTabStop");
    $r.addProperty("TabOrder",2,rtl.nativeint,"FTabOrder","SetTabOrder");
    $r.addProperty("Text",3,pas.Controls.$rtti["TCaption"],"GetText","SetText");
    $r.addProperty("TextHint",2,rtl.string,"FTextHint","SetTextHint");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,pas.Classes.$rtti["TNotifyEvent"],"FOnExit","FOnExit");
    $r.addProperty("OnKeyDown",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyDown","FOnKeyDown");
    $r.addProperty("OnKeyPress",0,pas.Controls.$rtti["TKeyPressEvent"],"FOnKeyPress","FOnKeyPress");
    $r.addProperty("OnKeyUp",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
    $r.addProperty("OnResize",0,pas.Classes.$rtti["TNotifyEvent"],"FOnResize","FOnResize");
  });
  rtl.createClass(this,"TWMemo",pas.StdCtrls.TCustomMemo,function () {
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("Anchors",14,pas.Controls.$rtti["TAnchors"],"FAnchors","SetAnchors",{stored: "IsAnchorsStored", Default: rtl.createSet(1,0)});
    $r.addProperty("Alignment",2,pas.Classes.$rtti["TAlignment"],"FAlignment","SetAlignment");
    $r.addProperty("BorderSpacing",2,pas.Controls.$rtti["TControlBorderSpacing"],"FBorderSpacing","SetBorderSpacing");
    $r.addProperty("BorderStyle",2,pas.Controls.$rtti["TBorderStyle"],"FBorderStyle","SetBorderStyle");
    $r.addProperty("CharCase",2,pas.StdCtrls.$rtti["TEditCharCase"],"FCharCase","SetCharCase");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("Lines",2,pas.Classes.$rtti["TStrings"],"FLines","SetLines");
    $r.addProperty("MaxLength",2,rtl.nativeint,"FMaxLength","SetMaxLength");
    $r.addProperty("ParentColor",2,rtl.boolean,"FParentColor","SetParentColor");
    $r.addProperty("ParentFont",2,rtl.boolean,"FParentFont","SetParentFont");
    $r.addProperty("ParentShowHint",2,rtl.boolean,"FParentShowHint","SetParentShowHint");
    $r.addProperty("PopupMenu",2,pas.Controls.$rtti["TCustomPopupMenu"],"FPopupMenu","SetPopupMenu");
    $r.addProperty("ReadOnly",2,rtl.boolean,"FReadOnly","SetReadOnly");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("TabOrder",2,rtl.nativeint,"FTabOrder","SetTabOrder");
    $r.addProperty("TabStop",2,rtl.boolean,"FTabStop","SetTabStop");
    $r.addProperty("TextHint",2,rtl.string,"FTextHint","SetTextHint");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("WantReturns",2,rtl.boolean,"FWantReturns","SetWantReturns");
    $r.addProperty("WantTabs",2,rtl.boolean,"FWantTabs","SetWantTabs");
    $r.addProperty("WordWrap",2,rtl.boolean,"FWordWrap","SetWordWrap");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,pas.Classes.$rtti["TNotifyEvent"],"FOnExit","FOnExit");
    $r.addProperty("OnKeyDown",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyDown","FOnKeyDown");
    $r.addProperty("OnKeyPress",0,pas.Controls.$rtti["TKeyPressEvent"],"FOnKeyPress","FOnKeyPress");
    $r.addProperty("OnKeyUp",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
    $r.addProperty("OnResize",0,pas.Classes.$rtti["TNotifyEvent"],"FOnResize","FOnResize");
  });
  rtl.createClass(this,"TWButton",pas.StdCtrls.TCustomButton,function () {
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("Anchors",14,pas.Controls.$rtti["TAnchors"],"FAnchors","SetAnchors",{stored: "IsAnchorsStored", Default: rtl.createSet(1,0)});
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize",{Default: false});
    $r.addProperty("Alpha",2,rtl.byte,"FAlpha","SetAlpha");
    $r.addProperty("BorderSpacing",2,pas.Controls.$rtti["TControlBorderSpacing"],"FBorderSpacing","SetBorderSpacing");
    $r.addProperty("Caption",3,pas.Controls.$rtti["TCaption"],"GetText","SetText");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("Hint",2,rtl.string,"FHint","SetHint");
    $r.addProperty("ModalResult",0,pas.Forms.$rtti["TModalResult"],"FModalResult","FModalResult");
    $r.addProperty("ParentFont",2,rtl.boolean,"FParentFont","SetParentFont");
    $r.addProperty("ParentShowHint",2,rtl.boolean,"FParentShowHint","SetParentShowHint");
    $r.addProperty("PopupMenu",2,pas.Controls.$rtti["TCustomPopupMenu"],"FPopupMenu","SetPopupMenu");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("TabOrder",2,rtl.nativeint,"FTabOrder","SetTabOrder");
    $r.addProperty("TabStop",2,rtl.boolean,"FTabStop","SetTabStop");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,pas.Classes.$rtti["TNotifyEvent"],"FOnExit","FOnExit");
    $r.addProperty("OnKeyDown",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyDown","FOnKeyDown");
    $r.addProperty("OnKeyPress",0,pas.Controls.$rtti["TKeyPressEvent"],"FOnKeyPress","FOnKeyPress");
    $r.addProperty("OnKeyUp",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
    $r.addProperty("OnResize",0,pas.Classes.$rtti["TNotifyEvent"],"FOnResize","FOnResize");
  });
  rtl.createClass(this,"TWCheckbox",pas.StdCtrls.TCustomCheckbox,function () {
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("Alignment",2,pas.StdCtrls.$rtti["TLeftRight"],"FAlignment","SetAlignment",{Default: pas.Classes.TAlignment.taRightJustify});
    $r.addProperty("Anchors",14,pas.Controls.$rtti["TAnchors"],"FAnchors","SetAnchors",{stored: "IsAnchorsStored", Default: rtl.createSet(1,0)});
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize",{Default: false});
    $r.addProperty("BorderSpacing",2,pas.Controls.$rtti["TControlBorderSpacing"],"FBorderSpacing","SetBorderSpacing");
    $r.addProperty("Caption",3,pas.Controls.$rtti["TCaption"],"GetText","SetText");
    $r.addProperty("Checked",3,rtl.boolean,"GetChecked","SetChecked");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("ParentColor",2,rtl.boolean,"FParentColor","SetParentColor");
    $r.addProperty("ParentFont",2,rtl.boolean,"FParentFont","SetParentFont");
    $r.addProperty("ParentShowHint",2,rtl.boolean,"FParentShowHint","SetParentShowHint");
    $r.addProperty("PopupMenu",2,pas.Controls.$rtti["TCustomPopupMenu"],"FPopupMenu","SetPopupMenu");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("State",3,pas.StdCtrls.$rtti["TCheckBoxState"],"GetState","SetState",{Default: pas.StdCtrls.TCheckBoxState.cbUnchecked});
    $r.addProperty("TabOrder",2,rtl.nativeint,"FTabOrder","SetTabOrder");
    $r.addProperty("TabStop",2,rtl.boolean,"FTabStop","SetTabStop");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,pas.Classes.$rtti["TNotifyEvent"],"FOnExit","FOnExit");
    $r.addProperty("OnKeyPress",0,pas.Controls.$rtti["TKeyPressEvent"],"FOnKeyPress","FOnKeyPress");
    $r.addProperty("OnKeyDown",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyDown","FOnKeyDown");
    $r.addProperty("OnKeyUp",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
    $r.addProperty("OnResize",0,pas.Classes.$rtti["TNotifyEvent"],"FOnResize","FOnResize");
  });
  rtl.createClass(this,"TWRadioButton",pas.StdCtrls.TCustomRadioButton,function () {
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("Anchors",14,pas.Controls.$rtti["TAnchors"],"FAnchors","SetAnchors",{stored: "IsAnchorsStored", Default: rtl.createSet(1,0)});
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize",{Default: true});
    $r.addProperty("BorderSpacing",2,pas.Controls.$rtti["TControlBorderSpacing"],"FBorderSpacing","SetBorderSpacing");
    $r.addProperty("Caption",3,pas.Controls.$rtti["TCaption"],"GetText","SetText");
    $r.addProperty("Checked",3,rtl.boolean,"GetChecked","SetChecked");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("Hint",2,rtl.string,"FHint","SetHint");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,pas.Classes.$rtti["TNotifyEvent"],"FOnExit","FOnExit");
    $r.addProperty("OnKeyDown",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyDown","FOnKeyDown");
    $r.addProperty("OnKeyPress",0,pas.Controls.$rtti["TKeyPressEvent"],"FOnKeyPress","FOnKeyPress");
    $r.addProperty("OnKeyUp",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
    $r.addProperty("OnResize",0,pas.Classes.$rtti["TNotifyEvent"],"FOnResize","FOnResize");
    $r.addProperty("ParentColor",2,rtl.boolean,"FParentColor","SetParentColor");
    $r.addProperty("ParentFont",2,rtl.boolean,"FParentFont","SetParentFont");
    $r.addProperty("ParentShowHint",2,rtl.boolean,"FParentShowHint","SetParentShowHint");
    $r.addProperty("PopupMenu",2,pas.Controls.$rtti["TCustomPopupMenu"],"FPopupMenu","SetPopupMenu");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("TabOrder",2,rtl.nativeint,"FTabOrder","SetTabOrder");
    $r.addProperty("TabStop",2,rtl.boolean,"FTabStop","SetTabStop",{Default: false});
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
  });
  rtl.createClass(this,"TWLabel",pas.StdCtrls.TCustomLabel,function () {
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("Alignment",2,pas.Classes.$rtti["TAlignment"],"FAlignment","SetAlignment");
    $r.addProperty("Anchors",14,pas.Controls.$rtti["TAnchors"],"FAnchors","SetAnchors",{stored: "IsAnchorsStored", Default: rtl.createSet(1,0)});
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize",{Default: true});
    $r.addProperty("BorderSpacing",2,pas.Controls.$rtti["TControlBorderSpacing"],"FBorderSpacing","SetBorderSpacing");
    $r.addProperty("Caption",3,pas.Controls.$rtti["TCaption"],"GetText","SetText");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("FocusControl",0,pas.Controls.$rtti["TWinControl"],"FFocusControl","FFocusControl");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("Layout",2,pas.Graphics.$rtti["TTextLayout"],"FLayout","SetLayout");
    $r.addProperty("ParentColor",2,rtl.boolean,"FParentColor","SetParentColor");
    $r.addProperty("ParentFont",2,rtl.boolean,"FParentFont","SetParentFont");
    $r.addProperty("ParentShowHint",2,rtl.boolean,"FParentShowHint","SetParentShowHint");
    $r.addProperty("PopupMenu",2,pas.Controls.$rtti["TCustomPopupMenu"],"FPopupMenu","SetPopupMenu");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("Transparent",2,rtl.boolean,"FTransparent","SetTransparent");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("WordWrap",2,rtl.boolean,"FWordWrap","SetWordWrap");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
    $r.addProperty("OnResize",0,pas.Classes.$rtti["TNotifyEvent"],"FOnResize","FOnResize");
  });
  rtl.createClass(this,"TWImage",pas.ExtCtrls.TCustomImage,function () {
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("Anchors",14,pas.Controls.$rtti["TAnchors"],"FAnchors","SetAnchors",{stored: "IsAnchorsStored", Default: rtl.createSet(1,0)});
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize",{Default: false});
    $r.addProperty("BorderSpacing",2,pas.Controls.$rtti["TControlBorderSpacing"],"FBorderSpacing","SetBorderSpacing");
    $r.addProperty("Center",2,rtl.boolean,"FCenter","SetCenter",{Default: false});
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("ParentShowHint",2,rtl.boolean,"FParentShowHint","SetParentShowHint");
    $r.addProperty("Proportional",2,rtl.boolean,"FProportional","SetProportional",{Default: false});
    $r.addProperty("PopupMenu",2,pas.Controls.$rtti["TCustomPopupMenu"],"FPopupMenu","SetPopupMenu");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("Stretch",2,rtl.boolean,"FStretch","SetStretch",{Default: false});
    $r.addProperty("StretchOutEnabled",2,rtl.boolean,"FStretchOutEnabled","SetStretchOutEnabled",{Default: true});
    $r.addProperty("StretchInEnabled",2,rtl.boolean,"FStretchInEnabled","SetStretchInEnabled",{Default: true});
    $r.addProperty("Transparent",2,rtl.boolean,"FTransparent","SetTransparent",{Default: false});
    $r.addProperty("URL",2,rtl.string,"FURL","SetURL");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
    $r.addProperty("OnPaint",0,pas.Classes.$rtti["TNotifyEvent"],"FOnPaint","FOnPaint");
    $r.addProperty("OnPictureChanged",0,pas.Classes.$rtti["TNotifyEvent"],"FOnPictureChanged","FOnPictureChanged");
    $r.addProperty("OnResize",0,pas.Classes.$rtti["TNotifyEvent"],"FOnResize","FOnResize");
  });
  rtl.createClass(this,"TWPanel",pas.ExtCtrls.TCustomPanel,function () {
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("Alignment",2,pas.Classes.$rtti["TAlignment"],"FAlignment","SetAlignment",{Default: pas.Classes.TAlignment.taCenter});
    $r.addProperty("Alpha",2,rtl.byte,"FAlpha","SetAlpha",{Default: 255});
    $r.addProperty("Anchors",14,pas.Controls.$rtti["TAnchors"],"FAnchors","SetAnchors",{stored: "IsAnchorsStored", Default: rtl.createSet(1,0)});
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize",{Default: false});
    $r.addProperty("BevelColor",2,rtl.longint,"FBevelColor","SetBevelColor",{Default: 536870912});
    $r.addProperty("BevelInner",2,pas.Controls.$rtti["TBevelCut"],"FBevelInner","SetBevelInner",{Default: pas.Controls.TBevelCut.bvNone});
    $r.addProperty("BevelOuter",2,pas.Controls.$rtti["TBevelCut"],"FBevelOuter","SetBevelOuter",{Default: pas.Controls.TBevelCut.bvRaised});
    $r.addProperty("BevelWidth",2,pas.ExtCtrls.$rtti["TBevelWidth"],"FBevelWidth","SetBevelWidth",{Default: 1});
    $r.addProperty("BorderSpacing",2,pas.Controls.$rtti["TControlBorderSpacing"],"FBorderSpacing","SetBorderSpacing");
    $r.addProperty("Caption",3,pas.Controls.$rtti["TCaption"],"GetText","SetText");
    $r.addProperty("ClientHeight",3,rtl.nativeint,"GetClientHeight","SetClientHeight");
    $r.addProperty("ClientWidth",3,rtl.nativeint,"GetClientWidth","SetClientWidth");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("ParentColor",2,rtl.boolean,"FParentColor","SetParentColor");
    $r.addProperty("ParentFont",2,rtl.boolean,"FParentFont","SetParentFont");
    $r.addProperty("ParentShowHint",2,rtl.boolean,"FParentShowHint","SetParentShowHint");
    $r.addProperty("PopupMenu",2,pas.Controls.$rtti["TCustomPopupMenu"],"FPopupMenu","SetPopupMenu");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("TabOrder",2,rtl.nativeint,"FTabOrder","SetTabOrder");
    $r.addProperty("TabStop",2,rtl.boolean,"FTabStop","SetTabStop");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("WordWrap",2,rtl.boolean,"FWordWrap","SetWordWrap");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,pas.Classes.$rtti["TNotifyEvent"],"FOnExit","FOnExit");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
    $r.addProperty("OnPaint",0,pas.Classes.$rtti["TNotifyEvent"],"FOnPaint","FOnPaint");
    $r.addProperty("OnResize",0,pas.Classes.$rtti["TNotifyEvent"],"FOnResize","FOnResize");
  });
  rtl.createClass(this,"TWTimer",pas.ExtCtrls.TCustomTimer,function () {
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled",{Default: true});
    $r.addProperty("Interval",2,rtl.longword,"FInterval","SetInterval",{Default: 1000});
    $r.addProperty("OnTimer",2,pas.Classes.$rtti["TNotifyEvent"],"FOnTimer","SetOnTimer");
    $r.addProperty("OnStartTimer",0,pas.Classes.$rtti["TNotifyEvent"],"FOnStartTimer","FOnStartTimer");
    $r.addProperty("OnStopTimer",0,pas.Classes.$rtti["TNotifyEvent"],"FOnStopTimer","FOnStopTimer");
  });
  rtl.createClass(this,"TWPageControl",pas.ComCtrls.TCustomPageControl,function () {
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("ActivePage",3,pas.ComCtrls.$rtti["TCustomTabSheet"],"GetActivePage","SetActivePage");
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("Anchors",14,pas.Controls.$rtti["TAnchors"],"FAnchors","SetAnchors",{stored: "IsAnchorsStored", Default: rtl.createSet(1,0)});
    $r.addProperty("BorderSpacing",2,pas.Controls.$rtti["TControlBorderSpacing"],"FBorderSpacing","SetBorderSpacing");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("ParentFont",2,rtl.boolean,"FParentFont","SetParentFont");
    $r.addProperty("ParentShowHint",2,rtl.boolean,"FParentShowHint","SetParentShowHint");
    $r.addProperty("PopupMenu",2,pas.Controls.$rtti["TCustomPopupMenu"],"FPopupMenu","SetPopupMenu");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("ShowTabs",2,rtl.boolean,"FShowTabs","SetShowTabs");
    $r.addProperty("TabHeight",2,rtl.smallint,"FTabHeight","SetTabHeight");
    $r.addProperty("TabIndex",2,rtl.nativeint,"FPageIndex","SetPageIndex");
    $r.addProperty("TabPosition",2,pas.ComCtrls.$rtti["TTabPosition"],"FTabPosition","SetTabPosition");
    $r.addProperty("TabOrder",2,rtl.nativeint,"FTabOrder","SetTabOrder");
    $r.addProperty("TabStop",2,rtl.boolean,"FTabStop","SetTabStop");
    $r.addProperty("TabWidth",2,rtl.smallint,"FTabWidth","SetTabWidth");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("OnEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,pas.Classes.$rtti["TNotifyEvent"],"FOnExit","FOnExit");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
  });
  rtl.createClass(this,"TWFloatEdit",pas.NumCtrls.TCustomNumericEdit,function () {
    this.GetValue = function () {
      var Result = 0.0;
      Result = pas.SysUtils.StrToFloatDef(this.RealGetText(),0);
      return Result;
    };
    this.SetValue = function (AValue) {
      this.RealSetText(pas.SysUtils.FloatToStrF(AValue,0,20,this.FDecimals));
    };
    this.RealSetText = function (AValue) {
      pas.StdCtrls.TCustomEdit.RealSetText.call(this,pas.SysUtils.FloatToStrF(pas.SysUtils.StrToFloatDef(AValue,0),0,20,this.FDecimals));
    };
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("Alignment",2,pas.Classes.$rtti["TAlignment"],"FAlignment","SetAlignment");
    $r.addProperty("Anchors",14,pas.Controls.$rtti["TAnchors"],"FAnchors","SetAnchors",{stored: "IsAnchorsStored", Default: rtl.createSet(1,0)});
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize",{Default: false});
    $r.addProperty("BorderSpacing",2,pas.Controls.$rtti["TControlBorderSpacing"],"FBorderSpacing","SetBorderSpacing");
    $r.addProperty("BorderStyle",2,pas.Controls.$rtti["TBorderStyle"],"FBorderStyle","SetBorderStyle");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("DecimalPlaces",0,rtl.nativeint,"FDecimals","FDecimals",{Default: 2});
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("ParentColor",2,rtl.boolean,"FParentColor","SetParentColor");
    $r.addProperty("ParentFont",2,rtl.boolean,"FParentFont","SetParentFont");
    $r.addProperty("ParentShowHint",2,rtl.boolean,"FParentShowHint","SetParentShowHint");
    $r.addProperty("PasswordChar",2,rtl.char,"FPasswordChar","SetPasswordChar");
    $r.addProperty("ReadOnly",2,rtl.boolean,"FReadOnly","SetReadOnly");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("TabStop",2,rtl.boolean,"FTabStop","SetTabStop");
    $r.addProperty("TabOrder",2,rtl.nativeint,"FTabOrder","SetTabOrder");
    $r.addProperty("Text",3,pas.Controls.$rtti["TCaption"],"GetText","SetText");
    $r.addProperty("TextHint",2,rtl.string,"FTextHint","SetTextHint");
    $r.addProperty("Value",3,rtl.double,"GetValue","SetValue");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,pas.Classes.$rtti["TNotifyEvent"],"FOnExit","FOnExit");
    $r.addProperty("OnKeyDown",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyDown","FOnKeyDown");
    $r.addProperty("OnKeyPress",0,pas.Controls.$rtti["TKeyPressEvent"],"FOnKeyPress","FOnKeyPress");
    $r.addProperty("OnKeyUp",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
    $r.addProperty("OnResize",0,pas.Classes.$rtti["TNotifyEvent"],"FOnResize","FOnResize");
  });
  rtl.createClass(this,"TWIntegerEdit",pas.NumCtrls.TCustomNumericEdit,function () {
    this.GetValue = function () {
      var Result = 0;
      Result = pas.SysUtils.StrToIntDef(this.RealGetText(),0);
      return Result;
    };
    this.SetValue = function (AValue) {
      this.RealSetText(pas.SysUtils.FloatToStrF(AValue,0,20,this.FDecimals));
    };
    this.RealSetText = function (AValue) {
      pas.StdCtrls.TCustomEdit.RealSetText.call(this,pas.SysUtils.FloatToStrF(pas.SysUtils.StrToFloatDef(AValue,0),0,20,this.FDecimals));
    };
    this.Create$1 = function (AOwner) {
      pas.NumCtrls.TCustomNumericEdit.Create$1.call(this,AOwner);
      this.BeginUpdate();
      try {
        this.FDecimals = 0;
      } finally {
        this.EndUpdate();
      };
      return this;
    };
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("Alignment",2,pas.Classes.$rtti["TAlignment"],"FAlignment","SetAlignment");
    $r.addProperty("Anchors",14,pas.Controls.$rtti["TAnchors"],"FAnchors","SetAnchors",{stored: "IsAnchorsStored", Default: rtl.createSet(1,0)});
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize",{Default: false});
    $r.addProperty("BorderSpacing",2,pas.Controls.$rtti["TControlBorderSpacing"],"FBorderSpacing","SetBorderSpacing");
    $r.addProperty("BorderStyle",2,pas.Controls.$rtti["TBorderStyle"],"FBorderStyle","SetBorderStyle");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("ParentColor",2,rtl.boolean,"FParentColor","SetParentColor");
    $r.addProperty("ParentFont",2,rtl.boolean,"FParentFont","SetParentFont");
    $r.addProperty("ParentShowHint",2,rtl.boolean,"FParentShowHint","SetParentShowHint");
    $r.addProperty("PasswordChar",2,rtl.char,"FPasswordChar","SetPasswordChar");
    $r.addProperty("ReadOnly",2,rtl.boolean,"FReadOnly","SetReadOnly");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("TabStop",2,rtl.boolean,"FTabStop","SetTabStop");
    $r.addProperty("TabOrder",2,rtl.nativeint,"FTabOrder","SetTabOrder");
    $r.addProperty("Text",3,pas.Controls.$rtti["TCaption"],"GetText","SetText");
    $r.addProperty("TextHint",2,rtl.string,"FTextHint","SetTextHint");
    $r.addProperty("Value",3,rtl.nativeint,"GetValue","SetValue");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,pas.Classes.$rtti["TNotifyEvent"],"FOnExit","FOnExit");
    $r.addProperty("OnKeyDown",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyDown","FOnKeyDown");
    $r.addProperty("OnKeyPress",0,pas.Controls.$rtti["TKeyPressEvent"],"FOnKeyPress","FOnKeyPress");
    $r.addProperty("OnKeyUp",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
    $r.addProperty("OnResize",0,pas.Classes.$rtti["TNotifyEvent"],"FOnResize","FOnResize");
  });
  rtl.createClass(this,"TWDateEditBox",pas.DttCtrls.TCustomDateTimeEdit,function () {
    this.GetValue = function () {
      var Result = 0.0;
      Result = pas.SysUtils.StrToDateDef(this.RealGetText(),0);
      return Result;
    };
    this.SetValue = function (AValue) {
      this.RealSetText(pas.SysUtils.DateToStr(AValue));
    };
    this.InputType = function () {
      var Result = "";
      Result = "date";
      return Result;
    };
    this.RealSetText = function (AValue) {
      pas.StdCtrls.TCustomEdit.RealSetText.call(this,pas.SysUtils.FormatDateTime(pas.SysUtils.ShortDateFormat,pas.SysUtils.StrToDateDef(AValue,0)));
    };
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("Alignment",2,pas.Classes.$rtti["TAlignment"],"FAlignment","SetAlignment");
    $r.addProperty("Anchors",14,pas.Controls.$rtti["TAnchors"],"FAnchors","SetAnchors",{stored: "IsAnchorsStored", Default: rtl.createSet(1,0)});
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize",{Default: false});
    $r.addProperty("BorderSpacing",2,pas.Controls.$rtti["TControlBorderSpacing"],"FBorderSpacing","SetBorderSpacing");
    $r.addProperty("BorderStyle",2,pas.Controls.$rtti["TBorderStyle"],"FBorderStyle","SetBorderStyle");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("ParentColor",2,rtl.boolean,"FParentColor","SetParentColor");
    $r.addProperty("ParentFont",2,rtl.boolean,"FParentFont","SetParentFont");
    $r.addProperty("ParentShowHint",2,rtl.boolean,"FParentShowHint","SetParentShowHint");
    $r.addProperty("PasswordChar",2,rtl.char,"FPasswordChar","SetPasswordChar");
    $r.addProperty("ReadOnly",2,rtl.boolean,"FReadOnly","SetReadOnly");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("TabStop",2,rtl.boolean,"FTabStop","SetTabStop");
    $r.addProperty("TabOrder",2,rtl.nativeint,"FTabOrder","SetTabOrder");
    $r.addProperty("Text",3,pas.Controls.$rtti["TCaption"],"GetText","SetText");
    $r.addProperty("TextHint",2,rtl.string,"FTextHint","SetTextHint");
    $r.addProperty("Value",3,pas.System.$rtti["TDate"],"GetValue","SetValue");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,pas.Classes.$rtti["TNotifyEvent"],"FOnExit","FOnExit");
    $r.addProperty("OnKeyDown",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyDown","FOnKeyDown");
    $r.addProperty("OnKeyPress",0,pas.Controls.$rtti["TKeyPressEvent"],"FOnKeyPress","FOnKeyPress");
    $r.addProperty("OnKeyUp",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
    $r.addProperty("OnResize",0,pas.Classes.$rtti["TNotifyEvent"],"FOnResize","FOnResize");
  });
  rtl.createClass(this,"TWTimeEditBox",pas.DttCtrls.TCustomDateTimeEdit,function () {
    this.GetValue = function () {
      var Result = 0.0;
      Result = pas.SysUtils.StrToTimeDef$1(this.RealGetText(),0,pas.SysUtils.FormatSettings.TimeSeparator);
      return Result;
    };
    this.SetValue = function (AValue) {
      this.RealSetText(pas.SysUtils.TimeToStr(AValue));
    };
    this.InputType = function () {
      var Result = "";
      Result = "time";
      return Result;
    };
    this.RealSetText = function (AValue) {
      pas.StdCtrls.TCustomEdit.RealSetText.call(this,pas.SysUtils.FormatDateTime(pas.SysUtils.ShortTimeFormat,pas.SysUtils.StrToTimeDef$1(AValue,0,pas.SysUtils.FormatSettings.TimeSeparator)));
    };
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("Alignment",2,pas.Classes.$rtti["TAlignment"],"FAlignment","SetAlignment");
    $r.addProperty("Anchors",14,pas.Controls.$rtti["TAnchors"],"FAnchors","SetAnchors",{stored: "IsAnchorsStored", Default: rtl.createSet(1,0)});
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize",{Default: false});
    $r.addProperty("BorderSpacing",2,pas.Controls.$rtti["TControlBorderSpacing"],"FBorderSpacing","SetBorderSpacing");
    $r.addProperty("BorderStyle",2,pas.Controls.$rtti["TBorderStyle"],"FBorderStyle","SetBorderStyle");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("ParentColor",2,rtl.boolean,"FParentColor","SetParentColor");
    $r.addProperty("ParentFont",2,rtl.boolean,"FParentFont","SetParentFont");
    $r.addProperty("ParentShowHint",2,rtl.boolean,"FParentShowHint","SetParentShowHint");
    $r.addProperty("PasswordChar",2,rtl.char,"FPasswordChar","SetPasswordChar");
    $r.addProperty("ReadOnly",2,rtl.boolean,"FReadOnly","SetReadOnly");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("TabStop",2,rtl.boolean,"FTabStop","SetTabStop");
    $r.addProperty("TabOrder",2,rtl.nativeint,"FTabOrder","SetTabOrder");
    $r.addProperty("Text",3,pas.Controls.$rtti["TCaption"],"GetText","SetText");
    $r.addProperty("TextHint",2,rtl.string,"FTextHint","SetTextHint");
    $r.addProperty("Value",3,pas.System.$rtti["TTime"],"GetValue","SetValue");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnDblClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnDblClick","FOnDblClick");
    $r.addProperty("OnEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,pas.Classes.$rtti["TNotifyEvent"],"FOnExit","FOnExit");
    $r.addProperty("OnKeyDown",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyDown","FOnKeyDown");
    $r.addProperty("OnKeyPress",0,pas.Controls.$rtti["TKeyPressEvent"],"FOnKeyPress","FOnKeyPress");
    $r.addProperty("OnKeyUp",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
    $r.addProperty("OnResize",0,pas.Classes.$rtti["TNotifyEvent"],"FOnResize","FOnResize");
  });
  rtl.createClass(this,"TWFileButton",pas.BtnCtrls.TCustomFileButton,function () {
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign");
    $r.addProperty("Anchors",14,pas.Controls.$rtti["TAnchors"],"FAnchors","SetAnchors",{stored: "IsAnchorsStored", Default: rtl.createSet(1,0)});
    $r.addProperty("AutoSize",2,rtl.boolean,"FAutoSize","SetAutoSize",{Default: false});
    $r.addProperty("BorderSpacing",2,pas.Controls.$rtti["TControlBorderSpacing"],"FBorderSpacing","SetBorderSpacing");
    $r.addProperty("Caption",3,pas.Controls.$rtti["TCaption"],"GetText","SetText");
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled");
    $r.addProperty("Filter",2,rtl.string,"FFilter","SetFilter");
    $r.addProperty("Font",2,pas.Graphics.$rtti["TFont"],"FFont","SetFont");
    $r.addProperty("HandleClass",2,rtl.string,"FHandleClass","SetHandleClass");
    $r.addProperty("HandleId",2,rtl.string,"FHandleId","SetHandleId");
    $r.addProperty("ParentFont",2,rtl.boolean,"FParentFont","SetParentFont");
    $r.addProperty("ParentShowHint",2,rtl.boolean,"FParentShowHint","SetParentShowHint");
    $r.addProperty("ShowHint",2,rtl.boolean,"FShowHint","SetShowHint");
    $r.addProperty("TabOrder",2,rtl.nativeint,"FTabOrder","SetTabOrder");
    $r.addProperty("TabStop",2,rtl.boolean,"FTabStop","SetTabStop");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible");
    $r.addProperty("OnChange",0,pas.Classes.$rtti["TNotifyEvent"],"FOnChange","FOnChange");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
    $r.addProperty("OnEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnEnter","FOnEnter");
    $r.addProperty("OnExit",0,pas.Classes.$rtti["TNotifyEvent"],"FOnExit","FOnExit");
    $r.addProperty("OnKeyDown",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyDown","FOnKeyDown");
    $r.addProperty("OnKeyPress",0,pas.Controls.$rtti["TKeyPressEvent"],"FOnKeyPress","FOnKeyPress");
    $r.addProperty("OnKeyUp",0,pas.Controls.$rtti["TKeyEvent"],"FOnKeyUp","FOnKeyUp");
    $r.addProperty("OnMouseDown",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseDown","FOnMouseDown");
    $r.addProperty("OnMouseEnter",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseEnter","FOnMouseEnter");
    $r.addProperty("OnMouseLeave",0,pas.Classes.$rtti["TNotifyEvent"],"FOnMouseLeave","FOnMouseLeave");
    $r.addProperty("OnMouseMove",0,pas.Controls.$rtti["TMouseMoveEvent"],"FOnMouseMove","FOnMouseMove");
    $r.addProperty("OnMouseUp",0,pas.Controls.$rtti["TMouseEvent"],"FOnMouseUp","FOnMouseUp");
    $r.addProperty("OnMouseWheel",0,pas.Controls.$rtti["TMouseWheelEvent"],"FOnMouseWheel","FOnMouseWheel");
    $r.addProperty("OnResize",0,pas.Classes.$rtti["TNotifyEvent"],"FOnResize","FOnResize");
  });
  rtl.createClass(this,"TWSplitter",pas.ExtCtrls.TCustomSplitter,function () {
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Align",2,pas.Controls.$rtti["TAlign"],"FAlign","SetAlign",{Default: pas.Controls.TAlign.alNone});
    $r.addProperty("Color",2,rtl.longint,"FColor","SetColor");
  });
  this.TPopupAlignment = {"0": "paLeft", paLeft: 0, "1": "paRight", paRight: 1, "2": "paCenter", paCenter: 2};
  this.$rtti.$Enum("TPopupAlignment",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TPopupAlignment});
  this.TTrackButton = {"0": "tbRightButton", tbRightButton: 0, "1": "tbLeftButton", tbLeftButton: 1};
  this.$rtti.$Enum("TTrackButton",{minvalue: 0, maxvalue: 1, ordtype: 1, enumtype: this.TTrackButton});
  rtl.createClass(this,"TWPopupMenu",pas.Controls.TCustomPopupMenu,function () {
    this.$init = function () {
      pas.Controls.TCustomPopupMenu.$init.call(this);
      this.FAlignment = 0;
      this.FAutoPopup = false;
      this.FOnClose = null;
      this.FOnPopup = null;
      this.FTrackButton = 0;
    };
    this.$final = function () {
      this.FOnClose = undefined;
      this.FOnPopup = undefined;
      pas.Controls.TCustomPopupMenu.$final.call(this);
    };
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Alignment",0,$mod.$rtti["TPopupAlignment"],"FAlignment","FAlignment",{Default: $mod.TPopupAlignment.paLeft});
    $r.addProperty("AutoPopup",0,rtl.boolean,"FAutoPopup","FAutoPopup",{Default: true});
    $r.addProperty("TrackButton",0,$mod.$rtti["TTrackButton"],"FTrackButton","FTrackButton",{Default: $mod.TTrackButton.tbRightButton});
    $r.addProperty("OnPopup",0,pas.Classes.$rtti["TNotifyEvent"],"FOnPopup","FOnPopup");
    $r.addProperty("OnClose",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClose","FOnClose");
  });
});
rtl.module("Menus",["System","Classes","SysUtils","Types","JS","Web","Graphics","Controls"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass(this,"TMenuItem",pas.Controls.TCustomMenuItem,function () {
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addProperty("Caption",3,pas.Controls.$rtti["TCaption"],"GetText","SetText");
    $r.addProperty("OnClick",0,pas.Classes.$rtti["TNotifyEvent"],"FOnClick","FOnClick");
  });
});
rtl.module("Dialogs",["System","Classes","SysUtils","Types","Graphics","Controls","StdCtrls","ExtCtrls","Forms","WebCtrls"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.TMsgDlgType = {"0": "mtWarning", mtWarning: 0, "1": "mtError", mtError: 1, "2": "mtInformation", mtInformation: 2, "3": "mtConfirmation", mtConfirmation: 3, "4": "mtCustom", mtCustom: 4};
  this.$rtti.$Enum("TMsgDlgType",{minvalue: 0, maxvalue: 4, ordtype: 1, enumtype: this.TMsgDlgType});
  this.TMsgDlgBtn = {"0": "mbYes", mbYes: 0, "1": "mbNo", mbNo: 1, "2": "mbOK", mbOK: 2, "3": "mbCancel", mbCancel: 3, "4": "mbAbort", mbAbort: 4, "5": "mbRetry", mbRetry: 5, "6": "mbIgnore", mbIgnore: 6, "7": "mbAll", mbAll: 7, "8": "mbNoToAll", mbNoToAll: 8, "9": "mbYesToAll", mbYesToAll: 9, "10": "mbHelp", mbHelp: 10, "11": "mbClose", mbClose: 11};
  this.$rtti.$Enum("TMsgDlgBtn",{minvalue: 0, maxvalue: 11, ordtype: 1, enumtype: this.TMsgDlgBtn});
  this.$rtti.$Set("TMsgDlgButtons",{comptype: this.$rtti["TMsgDlgBtn"]});
  this.MessageDlg = function (AOwner, ACaption, AMessage, ADlgType, AButtons, ADefaultButton, AModalResultProc) {
    var VMessageDialog = null;
    if (!(AOwner != null)) AOwner = pas.Forms.Application().FActiveForm;
    VMessageDialog = $impl.TMessageDialog.$create("Create$1",[AOwner]);
    VMessageDialog.FButtons = rtl.refSet(AButtons);
    VMessageDialog.SetText(ACaption);
    VMessageDialog.FDefaultButton = ADefaultButton;
    VMessageDialog.FDialogType = ADlgType;
    VMessageDialog.FMessage = AMessage;
    VMessageDialog.PrepareLayout();
    VMessageDialog.ShowModal(AModalResultProc);
  };
  this.MessageDlg$2 = function (AOwner, ACaption, AMessage, ADlgType, AButtons, AModalResultProc) {
    $mod.MessageDlg(AOwner,ACaption,AMessage,ADlgType,rtl.refSet(AButtons),$impl.ModalDefaultButton(AButtons),AModalResultProc);
  };
  this.ShowMessage$1 = function (AMessage) {
    $mod.MessageDlg$2(pas.Forms.Application().FActiveForm,"",AMessage,2,rtl.createSet(2),null);
  };
  $mod.$implcode = function () {
    $impl.DialogCaption = ["Warning","Error","Information","Confirmation",""];
    $impl.ButtonCaption = ["Yes","No","Ok","Cancel","Abort","Retry","Ignore","All","No to All","Yes to All","Help","Close"];
    $impl.DialogIcon = ["data:image\/png;base64,iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAYAAACqaXHeAAAJCElEQVR42u2aA3gk2dfGaxFzES\/Hth3bNoc7GRvB2LY9X8YzsW11x7ZtVON8t7PeSofdM0n\/5z7PL6jqe85939JzTjX2eXz08Xl8Hu9ssR9DnKT2Rp8a5x57atyH4P2SO97bYdL\/E+LfO3DPSL2zJLaj4AbQmwIRAdCedwVINxeGv7LBxnO0+Lc2mGjCxel+lJq3ADU3AYq3AJRsQ3\/fAbzSDWJOjn\/9whDj41gDAnd9a9tMPoRD3UOAovX\/YANAvRvUxu5o894mpMWR4p+twb5PubUoht7gCVC8HaBw3b8p3Qu0uveQeHFqgJc5JsxxBgTtFdvUknqMSq+6DvSCtb1Tcw8akvZ1+e0SteQo8e5WmEzq\/aVkWv07oBdtQmLX9E7xNnQWvAXSzXmRL2wxMY4xIHifxP62rFN0esV5oOev6Zuqq9BCdqYE7xHbyBHiX1hg47KeymfR616io7wBiVzdN4WOwPhs+t2lSa8dsB9GtXjAsC\/CXKRPtuecAXrZMaDnOQyM8jPQmnaYHnJQ0mlUG\/DKGpua90ojn17z5PfTO89+YOSvA3rtM8h+Ip\/x2g4bMyrFu7hgX0Yd+fF6Zz667oudgZ5rPzhKDkN75jEId5U5wziTRp0BH1ZzzS\/6oFdGr777hyi7wcG4FKofQP5rjeI39ti00XXXd8G+jj7205OugnNAL9wD9BzboVF0ALryzkDkkR9uvjDEvho1Bniu5V1R5m1aS6+4+ocYmyGC5lZehyJ33Yq39lwLRoV4L0eMJ\/rUmDfdheeBlr8VaNk2BBpCDOGJ8s\/gKsDVw2OFn6DaR5exj0jBTugqOAtRx39+lrAG4xr5R38Dj0pVoGUjvewsEmBNoCHEAA6L8MA+DPsXaFvPPuIcdCaUnYcyH6N6j028siNa\/CNLTAA1OHzxQiQ+9zegZVkReKL86x+iiTxV+ZU4h0GeI+AFZyD65Bj3F1tHcLnstUXQoCbEpo1WfAQt3LJXXAW4mRrgKsjDdB695BhUB1m0+mwV0h6xzY6k85NCKEWngJazBmiZFr3izMvF1ABnPi6m8xgxKYUnIf7sxIAXazCREWeA\/zZRu\/oIu25a4UHmIhAnJISZGnBSSqTPubRCJ6gNten02z7CymV31OwgX54WTS08jk5XW6BlmDPl4kQJpgZcmizZ11wU2w6ohccg+fLUiBFVLgft+m5TY5QDhZa\/Ey3UrE\/uLBnD1IA7y8b2O5+WvwsaIu0pgbu\/cxwxzQ7yzZnJ1AJXtEBLoKWb9skbkzlMDXhrPre\/+SiHFdAKD0Hq9dmJI6JcDtwrdqApbg2NmrsZqGiB\/RG6R5apAWF7ZQcUg5GrKdaBFrRf3OmTNzvS787PpOUfRAszB2qaSb+kXtNgakDadc0BxejJlX8AMu4tSH9rjY39dM0OZ\/GTzfFrgJq1Di3MeEBUeBgxNaDS02jAcRg5m+NWQ5iTxDkA7ItP0uzIfrgol5a7C6ipxgijAdFNsoQDX39JEI+2oX0WA47DyEnL2QXZj5cUMcrlj97siHCVutGa4ADUDDugphgOAiM4+8v3BAPOjRFj7BtcLJS7LdEBNU2kbhHLZTY3O\/KfLS2hZm8Z\/KIRD+UnEgx4pDBp0HF6cmdvhgK3lZWea7gWfrxmx2HpJ+2JduhatAQq2WDQeDksJBjgs3bRkGIx1tCe5ADRR2Weo7OA+6M0O4pfylZTM9f\/sQj9QRN3TIFgQPxxRca+oZGxFkreyNZ+2Mgrx\/5mx\/Ef33Qk2aDTzxSoJL0hkf9Yi2BA4VPtIcZDpJhAZ7IdxB7\/8YP7GoyffQas51EteyNXT0l3AApKPFQaggwJBjSGGA0rJmNNpe\/lW3w2CWmzrdkRc+Inn44kO6CQ0WKTdYcMTjICF\/6\/+wLobyRieDEZ87tI9hB36md\/tpTLvo6CBuXuCq2UVBuUUGfYXJwk+a8qkBUxKanWUOmp1Om\/TdiK5c2O+FO\/hnST7FAifaAkaQ+bx4p\/PwqfKE9iSUxKsh7gZDtIOjeGteWy\/zYhu2pv5S5KiiVKpMUSvOzn\/2WAt8N8lsWlpJhDra8KJWS3qCPLmh2kc2OjcZItSoCutUQtlhDtvOovA2JcVrEsLiVJB5lgC6RLk1hTLgfsEt1U56eKU8hmKIEm60i1B2re4R7Q36yNTTKBhgANWvCeb52H3ewgXZ6YjJOsf3c3QWOUgEwgW0PGzSlpwyqXUdvpAMNJSrIRCqo+ukg2hMZgLYhw+n5o5TKj2ZF6bUoGhWQJeLwGQn2UoQGMtWffnVX4zgabPvhmx8HvTjYEawKepI+CqbGUOg85eCwnCa78X\/fwaJUEVL5ayfI8eKIetEToQoSz2O1BlcuMZkfmrWk5eJI54HFqCFWWUecuC4eFCW+H0DYuxj6W5upZe7Ip5D2aSyyX+2x2OInfaA5FRz9BBwVRYSmP5aSYtsSeKEizPB+eoAWtUXoQfUiCUC4zbXbk3p9ZgieZAh6rilBhKX2\/G+QmzmEFicZQ\/H8Lar2I5TKx2RHpKv6kNVwL8HhNNFmZ1fRpwCEhHrbkxOM0oD1GH2KPSvddLnuu\/XpF4ZO51XiiIeAxSmzhscJEpgY8U53Mtrx4ggEUv1iCymX+3svl+zYYb9QhibdtEdrINTXAoxXZQpW7ORwW5iWIPyLKB3X+VmzLi8eoQmeMHsQek+n97bLnWh7FYrf5jXi8HpqgwD7iNKEu+Dd4qjK55\/sACHTkp\/RsQ8azN3e8DlS8Wdrht1nQkGBAhJPYndZQdfRBFcCjFNhLjDpQyOuBkrGvB\/Q32qbG\/rxIW1uEDkS6iD8iGIBufn7dCWbQHaWAkOdQkLZEC4hyEfclGBDtIva8IwYZgFzqjpTjTNBZ1h5pis4AsXsEA3w2CRqVv5XtwJNt\/jBBlnP4QzxOcoASt6X1Hut5FXptevpsF71f9k6xsz3OAXDyRsBTNnEGSEt7jC0Uua1ocd8ifOEmk+8b8kz9ARt3So9v1+vN3wR57vouxWv39xmcgAfS8mLTt35O6lyOv4hivzC0YkzGt4gxiKmIGRzG1D+0IY19j68RAggRDkOgR9u\/xufxefw\/CzT7sU6iahAAAAAASUVORK5CYII=","data:image\/png;base64,iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAYAAACqaXHeAAAMV0lEQVR42uxaBXQbxxbdz2QHZDnM5drl1iSOLTRzmJmZmZmZ+fMPlRvGsimKJGMSRSaRUaE30p9X+FBJIe+U95x7zpyZt3PfvQtvdiTu5+Png+0xh+N+O4Xjnpz561\/LNgkEg3a2bLl6d5s2BymO7GnT5j0EtrFvR8uWqzAGY2dw3BNbOe43P1TRAVSEaEeLFksOduhw8kRUVGFOaurt\/AEDiGXKFGJdsIA4V64kNevXI7CNfV+MYUw2jf0gKqrgQPv2J7Y3b754zq9\/HYVzfu+F0yvdYYNQOPFQp05nP9LpHDfGjCHVq1aRmsWLSdXkyaRq5Ehw9u8Pju7dwZ6ZCfa0tC+AbezDMYzBWDynhp6Lc1ymc+GcGwWCCdM5rv33Tji9XTuuFwrnHH3hhWxT\/\/6kevlyUj1rFnEOGgS29HSwpaQ8FvBc5+DBgHPhnKZ+\/cixl17Sbw4OXjCHmv2dC5\/EcYFrGjcefDg0NLdw0CBSs2wZcYweTWwZGWBNTuYVOKcD7yjKUTBwIDkSEpKztnHjQZjDd\/Wch+5p2+5Ydkqqy7FoEXGOHAXW1DSwJiUzBXIgl5Nyfp6U7NrTtu3RmRwX8m0K\/\/XygIAuR0ND9ZYJE4hz6lRipc9wZVLStwrkRO6b48eToyEhV1YGBGRibkzFr+O4361t2nTqObnCZp8zl9jo81mRmPSdAnPAXM7KZFaa25SRNEcm4idw3J\/WBwUt+lijrXbOmUMqu3SFioTE7wWsXbuBY\/Ycclmlrt0sFC7FXHm\/8huCgpZ8npDockyfQSroc1gen\/C9QkVaOjhmzCSfxse7aK6L8E7g7ZnH2\/4jtbrGPnU6KU9KgbK4hAahvE9fcF246CYulwfhunzZXTF4SIPnxdzs06aTSzGqGnwc\/sZxv2qwAfjCOyuRWHHi8pRUShTfMPTuA6SmxvPNg9TWenCsofNjjrap08gZkbhySUBARoNL3eHnnsurmDiJlNFbrDQ2rkG4qYuF+vMX3B4\/R925c26MaShPeXoGYM40dz3V8PxjL3J2t259rGTQEFLeqw9YdHENxnW1Fkh9vcffQerqPRjDBxfmXDxwMNnVqtWRx\/qOWBnQePAnak19xdDhxKKNBT5wTaUB9927\/g24c8eDMXzxVQwbQT5WqepXBQb2f+S1\/T+efjqnbOxYYqG19qZWxwtKlCq453T6NeCuze7BGL74LLREltPl89+feirrkT6iNgiC5l9JSyelPXqBWaPjDcVKNdw2m\/0acPv6dQ\/G8MlZ1rM3XElJI+sFgjkP++Lr8M+nnskrGzGKmOltZFZreUNxjApcRqPfl6DLYHBjDJ+cX5gwYiTBO\/qh7oJ1TZtOzIlPJJZu3eGGSsMriqKVUHP5sl8DqunaoJjG8M2LWrLjE8i6Jk3G37\/m0yXk\/rZtz94cPJTc0MTCdaWGV6A4+9Fjfg2wHTniRpP45kUtqIluv52+b0WYR7exzkRE2Cy9+tAT1bwDDajYuZP4M6Bi+w6CMSy4UdOZ8Agb3aqL9GvApqDgJYakVLiRmALXYtS8o6izEixLlvo1wLJoMSmmMSy4b1BdV5NSYHNQ0AKf4nEHdneLVifMffpDCT2hJFrFO4pQ3PgJfg24NnYcwRiM5R30UbhJte1q2eoDn7vNuHV9\/PmQfHP3noBXgQWKFDFQ2KM3+DOgqEcvjGHGb+7eC44\/F2LCLXcvA3Av\/kKU+Nb15HQoViiZoEgeAyalxrcBbrcHxzCGFT9qOx8pctFtdqmXAWvo0vfTaCUpiU3Aq8AEhRQGqRzuOhzeq0C73WOUKjCGGX9JXCJ8qoghqwIb9\/d+AQqDV+tj46BIpYVCeTQzGCVyqNPrvUph3RW9G8dYcqM2vS4ONgqEK7wM2C5sdig\/MRWKOqugUBbNDEaxHBwnTnoZYH\/\/AzSAKTdqMyWkwBZhswNeBmwTNjtaSEtFAXWqQNqZGQzUgLJ9B7wMKNuzz41jLLm\/0EZL\/FZhs8O+DPigKDkV8mWdIV+qYAaDWAbmpcu9SqGZrg9wjCU3akON24Kbve\/bAHp75EsUTGGkIgtHj\/MyoGj0WGISy9nyS6kBCcm+DdgqFB7N1yWAiQZhIqxgFMnAmNnVqxQaM7rgGFPuL7RRjVuCfDwCWwTCQ3qlFozUKUyEFQwUuZQD6\/5\/DtrGPhxjyY3aqEZqgPCAdxkUCFdn4UJFGg3GKBkzGChyw8Vwx2r7j\/47lZUe7MMxltwmWQxk02qwsYlghY89wIDBl8OjiEmhpIlIWYIaIILa3Nz\/3AI1OTlu7GPNi9ouUY0+9whncpzs9Euvuoy0Vl6NlDAFirW+8+5\/DLDRNvax5kVtp6hGqlXi82PocKenjCalDq5GSJgiN0wElh27\/mMAtrGPNS9q+2fHJw1Uayefn8PbaSk0qnSgp8H6cDEz5IRFQcm8hf8phdjGPpacqMmg1MK2oOD3B\/n789XGxk0XfxwphatiBejDxMyQ94YIPn45DM6FvoLANvYx5URNqG1dE8F8zt9Bnw3RO8+GVhoUKnqSiBnyKLJfj4Cs174AtrGPKSdqevuZ561zfvObiPtuim4Pbn5GT4PzwvFKRf0ogFpQ0w5h81MP\/JlsTWDgxHOvhoFe3BlyX4\/6UQC1nH017N7qPwWOe6j\/+h1o3TZXL1d+OcFrkbzDoE2EqpOn3UB\/DEVUnT7rNqVkMuFCDajlQKs2WfTqt+Me5lgb0Gj2+dfCIC9KBjmvRfAKvTYe7lVVe+0I3auu9uAY33yo4dwrrxPU9Cj\/C+iwt2WbrFxpDGRTB7NfjeAFWa+Eg+PEKb8\/jDjeP+HGGL74MPccaTT8u1izgG0cicLwLAXsctUrLucaZidl5qorOGYGwTEzMzOz4HiZmTfkZlF0wmPG5R239\/7WkY5h4mhH+iRr4L3\/PT9PwPNGRZV6Pe7+\/2mPWKSLVjk9e3c2tfPtEGUAqj\/C+d69o3\/X+C97RzHHKH\/QvoJieMginSd04Pm5otIF8QhVAH2OpgPRrEn6lH9MwJGffxnFHCN8QTO0P19UOvdh0VNjOIH5Vnn1jnRDO5VvPR\/2R7Mi4VX4N\/g\/8G\/a1\/SbAHOy9ZMONXBofrOiZrd+REa83W+ynjxvhu2rdGM7V\/UyFoQSEOapwTn88F9sgofp4ESsoxtzsvIBjWkq\/bkzZn95v8l0PMu24ajZw1b52kU2x\/fbG9vGnfgUIVIU3DZ3gG+j\/+a+WrZi5AidDANfLV0+srW1g8doDHNE7asofdK4eHbtj49Y5eugnRnRcOiQvkTct8zu3pdu7OApf5SEKkLEPZQEZ4BvqPXwNTYnwDX6MCZsF5qgbSlpfJS0UumbDD8q+5iUf+9Cm\/2HYSoxOEySYBES7hCPOYO\/BX3C9qAFmhbZ7D89IeffT8FLLAdtYi8l4QGLdNNc2hPUpk6uhpt40h0+qkBDirTMnT77ywct8o1zxoOfaHjwxGTC5KMk3GoynfZqecWObfQjY7ixAx9vuIN\/jSuoZcXf2IVP+N5KGl4rr9xzs8lyejlpg0ZoNTIJE4hJxBTCTFiJvPMZiz5ZULxkqcOzT23r5ikSEqcNLO4O8rgroOUC2IYP+ILPxXb3XtKw6BzGItCkazPrWqF5Qs4SQBQ0MFZ9s9l82ctlFTvXBBRtuL2Pp6LNJDSkxZ0BQ4FNNdoy5mONX9FeKivfeYvZfGkTY1XQIpYAgUeAsBBSJglESR9Vw01m+bEXysp3rfGHj6gdfZra1KUlgvVajMo45vALgbWwAVuwucoXosArPrrdKj\/ZP37XS34TPDRZxB4B8STIRCFRTkxvZqz9KrP10ccLS1IfzKr9YWOkQVO7B0h8r5ZqaNUSSoMWD0S0mCekbaOS3ub0j0PX6MMY5mAu1mDtBqVee39m7Q+Pks0rzdZH6skHfMGn7lsWCN7QRFh\/k4RpxLGVjCm0G59znSS\/8UBBUeqN6mmfLLJ7Dq2i0t1c36rF23rGgkv3DgFcj\/VtoTHMWWh3H3qtauonDxYUJa+X5NcGyRZswjZ8\/CZ4q\/GBi+0NmWQUEccQNcQswkG3JtjA2NAQYxdebLI8cKNVfucOuWDVPXn5m+6VC5IA1+i7gcYuMpnux1yswVrY0G3V6LaLMkGLP+u5TchkXWABUUqU6+Kn6YHYCLse2G+x62Oz9Lk1+tpS3ZYFtnMfcG4Sk3lsMp8i8m82UYBr+Te7uF7OuQ\/0V0d+0gAdXWmtAAAAAElFTkSuQmCC","data:image\/png;base64,iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAYAAACqaXHeAAANXElEQVR42u2bBVQcSbfH572N+7cR4p51jUAWXdxdgkSQbOSTyPrG3XF3d4cYsnF3NEIcm2EGhxnkVs+rS2bPi8wK0P15nfM76XTfuvf+b1dVyzQ8jtt\/23\/bhymDePPD5g74PEJDwSxz1fQlJz1nLiuIp2TNXJqfh+A27ptmd8IDbdCWtyB4DmXgv6ho\/xEDFoapTLU7fuC9lWd+tna\/9+ibNEHnvsJmJvh6B4kt6SYp9wjJqJD2gNu4L4geQ5tv0gUdVh73Hr3reqpwqu2x\/QMWRSijz39+4Z8Fzpxglv3dx2sunF0VXdnofUlMUu4zJLJMStxvSsnOy1Ly\/TkprDslhbWFUlhVgLzYxn14DG3QFvsk32OI92UxWRX9vPEj6nOCec63vM\/DZvzzCf8kdNZEi+wdGltuFu3KayYJ5Qzxu8OQTReksLqAAbe8voF90Yc\/9YU+d+U1Ec2tt8umWB\/dg8X+xwtXCR\/5J4PU1ao\/XS\/ZQ4duDD1re64wZA1N3PUku6DPvdR3DC0ExlL+\/lrx23rpqzCHf4z4hWEfzVtemLs+WSCJLCZk5yWGrKSJupzgFoyBsTDmumS+eN6KwhzeopAP\/37Cv9wxYJRekr36plvl3pclxPM6IWvyCbgc\/\/uylsbE2J6XOojapptlo7VTlmBu3Io39Bk83iTzJ8eAJ6Kg20A2nWXA+Rj5h7LpHANBt4As8XskHGuU9SNvrs9gjha6mOEKFpn73CKrWvxvEfLnfIAVx\/45+EsBgD89IW4RVa0TrbIPYq6sn\/mJljkH1sTXSLxvEPLVCYDlR\/+5WHUSAHNbHVstUbDI3sfaSMB5NdYo86cV4ZUtHteAuB7rhmW57PPtz12w7VwXcTvedx+Ym8d1IMvDnrWMNcr4kWeb8la\/9Y\/STrK38q4QuV99IX5pDrt8dawTSvjdjFTWGsREuu2MhPTVnxvN0f1aN7HwrBCO0Ey26\/elTvG7ayWHLnWSVce7YGk2uzhlSuBqZSeKf6XVtnRLnTLa++x3FR1FmLPSd1fLeUoRH\/T5Jmf2soLcbfltZEN+NzhldbHOX3ObgZHKb+uPNffLN+a8Ja+VzHLKy+rTc8SfdFNXL4+oEm8+1U0cM7uAbRwyOmD\/qUYi\/ZX2lywR2vQrxpYz3WRZWFX72wbpbr2+t5+\/8XLxrnOdZEVOJzhkso99Wjt8n8uXW4CnIjFjE1ML9hmSfsVYTnNHDQvWX7rdq4coehnZvTZZRP56sosm0ckJS1LbwSqyCkqqWl6ZBW0dIF2XcI\/YJNSzEmddXhesoVom0Ie1P\/xIu2DjldKtZzqIIx2C9ukckSYBmzgRWPoVQdylKubKw0Ym40YtszzkDlhGVIJdcgsrcVADalm48WrRHxoF44yyv3OOrYM\/H6dnKa2DU2yTW8EqWgAm\/vfA0OsOGPuVg2VkDdgkNLIaB7U4xwpgnHH6N797uzvHufDc93kS4kDPkF0q99gmtYJ1fANY0dFgHVcPNkktrMdwSJcAaprrXHj6N68I+BrL8PAD0V9pxWxTJP9WoCb9gw9EAxTDv\/jVAky0PnrAJaEBltN5Y5Ms+bdiRWYHONOFVcH86B654vEN7AyngsK\/HRODXYqYdvr74JDSDlsKxcQlk9s4qAm1UY0Fct8246trpR9uP1iTKwHrJDHn2NKkfC6JiaAVXlz+Ool0XU4LpzFRm+IPN+\/xPgqe84Z+fBev7\/5YsiJDDFaJ7Zyy\/2w7qWzslr7e8u+3MZZxzZzFdaba9I88lAz4PEz9zVtfg\/TVFoHVxIEOFcuEdk5Ykd4GF5\/iw4\/8dut5G2MWJeIsvmOqGMwDqsloPTm3xgoWRz3to0X0stQGFvHss6OglTRJiPS3WtatOsY0UshJfMSOakONEy1yj7xRgMnWJxKWJTaDNRrHsYd1XCsk3m5nGEb6u21r1lNiGiHEfpyA2pYlNMEkq+NxbxbA9nj2iuTWHkPzWHZwTWuFcn5Xj\/SnQrF0e9p9si66RO4wILRCNqFPwDSqEftygiXVhhonWx\/PlFOAEwUuaWjYCmYsYBJZDw\/qupiOLiINKHhKtA9eBV3v+2AX\/gTkFeB+bRtjGFwFpjHN2J8TUJtLGi2A7cl8uQVwpgUwi2kFUxYwDOHD2uhy4hpyh2h7lIFBcDUYhQlh50mR3BGQfLWWMQoVYF\/OQG0rUn6lABNtjmYvTWyhhi1gEt1\/UKyu7yPQ9XsMhlSYcVQTGEfUQ9qtRrmrwY9pTwj2wb5cgdqc6Do3Sd4UmGR9LME+ls4\/LEAUO1BBFNH\/\/z9UCPf54jcK0NlNpCb+j2iBGtCOM1CbfUyD\/EVQwSzH0zq0Dsyjm8E4khssw\/kA5M0BcP1RI6MXUEkL0MRZbMQihuYQJoAJ8i6Do+mvvAZez4klrZJRRDP7hDfBphyh3PnvX1hJ9ANr0Y5TrKg2A89nZLR2upvcW2GNvRVim9hmMKTJso1BiBDirtbLnf+ukRVEP1iAdpyC2tT3PhDz5oeqyX0Y+mTD9ft28c1gQI3ZRi9IAMVV7W8UQNTSKdX2egT6oSK045QlVNvH66\/f5X0cMFvu4\/AUu7wC+zjZGQtjF6OgWsDF7vV24k4do+NXBQahjWjHGajJPrap5xL4qx9fjTfP2m\/sVwOmEU30jDSyR0gDrE8VyJ3\/u7OfEB3\/GrTjFLNIusD6VcNYk8zdv\/5K7LMwFcVNpUKraDpkQxpZQzdICCHn6+UWwDqwAnQCBGjHKahJ8adi0UClyMW\/+VJ0qn3eGevIBtqpAXSD2UHbnw+XHra8Mf\/5TR1STY\/HoBMoRDvOQC2oaeqSvFO\/+zPZaL2077QOPQHT8EbQCWpgBS2famgSv\/kC5OdSIaPp9fwVW+MwWVwWQS2aBx93jzFI+\/oP\/TAyx\/VssXU0rV5QPeiwgHNsjdwHIJ+850TTp6rHxiKiHk6UtTHPG7qkOgF8VuIiqAG1zFt5\/jZd\/Kbz\/kgbY5S+XefwM3o26PANrO8XWv51cDivTu78P3iskmj71sDBgibSKH5Rowv3GhgDv0rQDhD1OzbSo+HwE0J\/IN3eqy8+ZzmfvW0ZTquIIgL6zpe+tXCyRP4DUDdhpM2yt0TiTpDuz7hPFm+9DBruT2jhRP2Ki2DuFlTDTOczN3vOfm\/aSM2UVYt33GszCa0HTZpMX9HwqoLnIon0t1ppZQtj5XEVlPfcBnWPZ7RofOzbbzB3pW13W0dqJrr26YNnBcvj2fo+9BodRM+kn6hPqHk8h4radrkjoFXSLT2U\/YAobb0AKgfvU\/GVVLwQ+\/UbzFmPLr5UQ0afvxrDLzBnOZ8pMgkSgra\/EJPrNWruz2BfVgV5WXh7B0gjTz9ldHdfgC923QK1w4\/pSKlBe1bAXDHn2c5nSmWfyPS9DVVPXPLxhlt1piF0WFHnGj69AQtQBYt33YYfYktI5tVqxvf4I6K58zwdmtdA5UAFPV4JGt4CtGUFzBFz\/WjDLf5Q5QRrXn8bfmpG14Pv5v9Q3GASIqIVrgN1n15AxakcfgJKtAiLtlwGxR3XQfnAA1A98gzUvGrQhjUwN8xx\/g9FTSN107\/H3HlsNPzokL4v2Ke4tawdq6tBg6l59wIvWgQ6ElSOPKf\/VoKqZy3uZxUUj7kpbiltx1zxL1VY\/1R2pH7q3gU\/FDeah+JQqwNVKuyfAcwFc1rwY3HzaIOM\/fSSN4zHQftfnsKy4SM14zd9suFmnUVIHej50yJ4Cv6hYA4WwQKgOQlGaib8xJu8ahjmyrp4ygDKIJ7CJ8MHKQY7zlr+c5GRTzWYBglBHYe4h3yU3fmkP\/yaXw0a0yxYCEbeVTB7xamyIYsDnTA3zBFzZbMI\/0N5izKQMpgylDKC9+FmxfGmWUcXby1tt6ajwQBHgwcfBcMXR2oJF6BvjIGxMKbSlrK2caaZubx3flyEOclyGyzL9S3MnbMCUEbxxmpNGaocvG6GU36JzoHHxDZcCIZ+AqJMk118mF3QJ\/rGGNr7H5KZTvnFQ5WD\/sYbpzsZc+GmAHKmAGUIZdgvRaC8zZvqqDhcLdBjhsPxEq19D7rtQgXEPEhAtL3xzNUQpcN9A\/uiD\/SFPrX2VpAZS\/MqRqgHe\/OmLVmEsV8SPwxzkz8FuCvCcMpoigJlBm+iwZfDFQ+6K5in31jw7Y0mI\/enxD6sjtiE1hGTAAHR8+ETTc9aoupOC0MFLj70AtzGfXgMbdAW+2Bf9DH\/mxtNCuapN4YrHTmCMTCWLCbGHt4H8awWYuhLRZhOmccbNnshb9oy55EqnlFjDeNvvLfyVKXy1uJOnf0VxMz7GbEJrCb2oXziFCFAerZxHx5DG7R9d+Wp52MNE66PVPaKQF\/oE31jjJfED2VBOCtrwy\/FGEOZQJlKmU15jzdwzOe8cXomvOnLvhr6+bYDo9R9k0ZrhheM0Yk6P0Y35noPOtHncd8odZ+kIZ9u34+22Af7og+Zr6ky32N+Ec3OXGe\/IANkCY6ijKUoyJKfLhMyl\/IuCnuNd2XHZstsp8r6jpX5Qp8DuBfMRWFk0+alq8jwlxZRBLeHv7SKy4Yz90L\/D1wmRcCSWFt6AAAAAElFTkSuQmCC","data:image\/png;base64,iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAYAAACqaXHeAAAN10lEQVR42u1bBVhbWbfNe1OnNkrH3b0CDFQGdwIEq1BqQzvuU3cv7u7uVYrVvSUV6mgFSwgJmiD73LyzKaOEdkhyad97\/\/m+VeDec\/Zaax+7Vs5\/yn8Ky+X99GGciZFvDPk0eoamTY77S855Pq+4FiZR7HxlTkE+An\/HYy867ffGOliXMynsdYqh\/0tNB40eMjlS7wWn3G3vLDp8gOd1o+LnTEHnlqJmJuxcB0m43E3SbxCSXSZH9PyOx0LpOazzc5agw977RsXbCw4WveC4b+uQKdG6GPPRN\/5JyCvP2Oz69cMlx4+4x92V+J2UkvSbDIm5KidefDlZf0pOfjsqh+8OyuHLIjm4F94D\/o7H8BzWwbrYJu0GQ\/xOSYl73B3JBzTmM9zdv3A+jXz50TP+UcSrE2x3rZuxin9pQ34zSb7GkMCLDFlxXA6LCxlYmK8csC3GCKKxMOaG\/Caiv\/rC1ed5ezdhsh++cb2oMY+bZSyeuvzc5U106MbTXtt0miFLqPAFeeoFxtxMY8fTRCCX7m9nS54wyXJHDQ\/H\/OTID96cW7Tn+zSBLKaEkPUnGbKICp2\/n10gB3Ih53dp9dI33Yp2c6aEvz94xj9fN2SsSarL9BXnr\/mdkhGfc4QsKSAwP3dw8SXlRG6fkx1k2gr+1XGG6c6ojV3z5v7Dn7bKWT4ruEoUegHIiiMMzNtHHipWHGUg9DwQ58CKhictdi7jvOE\/nKWFLl5D0zZny8KY6pag84R8VQDgtu\/RwNeFAEG0QxZGV7dOsN+1HbWqvecn2O3etiSpVuZXTMgX+wHm7n204J4HgNoWJ9TING13bcGRoLY5\/6RFznK3qLst3meBLNjXDa57Hk2gNu9zQOZG3m550iJ7Gccx\/TGV\/Y81THWx9ysTeZ25Z37ObtWweH83bDreRaIudpGUq10k83oXvQrsIkHFXWT1kS6VORbS9l5nu4mtT1nDaP00J5W3Oq1fz17ecbKTuOd2wZxdymHh3g6Iu9hBboqAIYz8vqULGPn52m7G41QHcVWSzz23G1Cz9q9nrnG0o99T+iLnNdfCPWsK2sgPBd0we2fXwJHTCeF8GWntpK6VKOWNwPxWICXKcKPmVfmt5NXZ+TuVuo943Dhj8dzoaunKg91kVk4XDAxovgOO31LsXNTaKT96s5HJ5tcxB66JmGqxrN8kdHQz8m3HpEppWHW4m7hGVrc\/YZa1cMDX9hN\/PFWy4WgncdvdCTNzBgaXbBnkl8n6mG+Vdcu355YTS5+zYBlwEayDr4JV4CUwp3+vyrpBaiWyfqfF0vwWMlAdc6l29DDp+5MXBnQTRbeRjV+micg3eV3UTOeAsTSvmTD\/sI9\/\/5xyjdiElQMvUQgOyY3gmCLp+emQ2ADciErgBvDh4p1mhaOmorGLccpoG7CW7\/K7YAn18gy9WfvXt7STfjx9ZfXhDjIruwNcsgYGp\/RWKCqV9jFx\/lYzYxt5GxyTxeCcKf1bG\/wbk8GLF4BjSAn0NxJ+y5WQgepBD+hl8o9nLv2rUfCUxa5f5yUI4avcTiqsY8BwSJbAlRoFCbjdyvASReCcIeu3rWNqC9jH1kPUsRqiKAGBx5uIU1rbgDWhl3kJAnjKMuvnB17uvj6v6Ohv+TIyM1MGThkDhz01eaqi7zCWdhE5nntQe16SGDwL6xQmIOyEiDikNA9Y08wsGaCnN+YVHbrvjoCPscw9SkXf0Iw5psuUgn1CIwQf7NuDnd00AfEN4JgmvW97XnIzxJ0VK0zA9oJ6gueV0YWeTLeXioZoRX3WbwIm8PZum0\/n6NxsOpTTZErBLlECdmEVUFbf\/rdRsPt8PWMb2wAOqdIHtr9S23cHwQPzEu6AfVKzUrrc6LY8jy64mty9mxSaxyewL88uLPp2n5QuZChSOfBS24AbUw82fnwILLxFcorrmG17yom5fwnYxose2P6HXYp7n1\/VxNhE1QAvpVUpXegJvVGPhQqfNuOja+2lF0qX7JFRE1JVQHuxCWyia8E88AaY+tJ9PqiUJkVAe6\/lvu3w\/KFyad\/ep0e+T7pJuLENKulCb1pL+Tc4H4S93sc\/Pos39aqUuWVTISntKsOOmuHGif6AXXLrA9ss3SdReKuQfqaGsQytoiNIrJKmedSbqWe5bMinkdP7XvqaZS22DakhM9OlVGz7oMOejpoKUVcf\/yfLxIypbwlYRwtpUttU4piVIQVucA0ZZ6Lg0ljTdq+PC+0pRzqHbZMGGYmtEHW2tc\/cv1Hbylj6XgCryDrgxjepzONEvaHHCbZ7PPsk4Dne\/mRXusfyegQNLhZliKGj++\/+iyubGCsfPliG14BNrFgtPOjNNbkJnrXPTeybAMfcXW5prT0VuQmDBmquCY5W\/H3hKygRMiY7isEirAasYxrVxmVHvaHH53i5OQoSsL9wfiZWbAWbQcT3OY3kr+6TTtxlDHfwwZz2vBU1r04u9DY\/kybAMa9AYQLm0QTYxLeC9eCAGhTDico\/e3\/\/JQFj4Em3zbBaek6idj705pbeTwImOOzdNSelhVZsAau4wYFzvBC6gfnjIYml70UwC68DS2qeDT70Npuuc88qmgLP8vYluyRIgIsJiGUfltESCDwmIX\/c6By4Q0yD79LjYtY40ZtLvFjxIqhps9uHFyEEblwz7QH2YU65jpS2\/DH850aWglmYgFVO2\/hmsIsUwDOKtsFx9C2vme8dYkezZBHdzDpMQ+uhtP7e\/Be3dcmN\/avAPLKRVU576s3M5zYZZ5i1UOGl8IzNZVKHBNo7UU2swyS4Bu42yuRYzpRLGOOgapoACauc6G365lIpZ2LENIU3Qx\/9cO6mU1IzmNHKbMOIGq6VdMixHLouZoyDa1nndKbePvz+3HXOh8GvKbwdft4pv9Al8V62zCLZhVFgNVQJ2+VAGHlWsZAmoI5VPvTkktDUswX2+\/HV09ydWy0Da8E6uglMIySswjDgLuitPwG6q4\/AtM18wASwyWcT0wSWgTXwpFXOxv4fiX0Sqae14kqDfRydo+ESVmEUVAf6vrcoqsDA7w4YBQtY5UNPWstLREO1Y3Tu+1D0BZf8w7wYMW0kBuMwdmEUKgKjkAYwDm1klQe9oKcXnPMPPvA12TiTzF8NdlSBdZSEChT\/nwB60d9e2T3eLPOnf\/Vi5PUFR0p4ceKenjFiCS4JYvA\/2kJCT7aQn3aKCVs86AG9vLno2AW6+L3E+TdlvEXWWiOP22AZKQbDkEa1Y81eMcGXnn8th8ukjEmIUO1cPR48qgh9Qbp2QF98vjrvyAW7KJpFGsQgWH3ghteDtJPIFZWgoxKiH9SgNi7Ubks9vDLvML+n9wdSxuinu+usu9FmFdEI+kEiteDzQCFszhWie4WlpLqdmeFbozY+1K695nrrGP2UBUp98Kxpl7vL1L8WTEMbqXiRypjhVwfrd1X3mwB+ZTMzzfuOWrhQs4l\/DVAP2Up\/NYZfYL467\/Alq9AGMKRD8\/MA1TDDtxZMPK5BI733V1Q2ZJWTaV63VeZBraj5tXmHr\/R+IqN8GTk9xfnDH84LrcPpsEIT\/spjum896G0vh3lBfFLdKP3be8OQgkqis+ECTPOqVokDNaLWD344Xz9SN5nHUbXgp2Z0Pfh14tISsVW4iGZYCNP9lcdUrzugs\/kqvSo7BLP9zxL3MD6ZuvowaK05A3oeVTDdr17p2KgNNU5ceqlpjHHWb6ido46CHx3S5wVbtFZfbcfszqBk0\/yUhK8A9DzvgC4dCTqbroL2xhL4bGsp6Hrcgqk+dUrHRfOoTWvVlXbUiv9TRe2fyo4xzdg8aWmJhBuBQ432JjWjLNCsnlf1PXjXqhQLtaCmSctKmseZZW+lW94oDgvlvzmarhpj9JNWfPQDX2gbLgSTICE1InioQA22YQKgmgRj9JOXc55zH4Va1W6eYgjFMI7mRxrDtMJmvTr3wCULus1Yh+LiJqC9qBi6XvVEFfQXdwbltAlrAAu\/anjN7eDVETohs1EbakSt6kzCf1E8RjGUYjjFSIrRnPdXaj1tvXOvzuor7Tw6GsxwNHjXo2H4zLOOsAGMjRzIhZzaq662PWWds4fz1rIpqKlX2\/BerY+hdtYSQDGW86TB8yN1w757eXbBZaNtlcQxqgHMAwVEl4rV8VAvMCbGRg7DreXkldkFJSN1Q7\/lPGX8HGphKQF9pwDFCIpRvYRI\/ATnhVlaGtNCvF+emXvZYEtpt1OEgHBDBcTQD3uulmh7KAdsizEwFsY02FxGXp6TXzZ6epgf50XnKcj9F\/OjUJuiKcBmEjQoxlFoUrzMmWD2uYbWdi9NblbxpF+Kmyy8bhGXSCFxiBASq2ABMfGvJ\/o+dWSqF00MNaiz4x7wdzyG57AO1sU22BZjTPy5uEmTm1Gsoe3piRzI1cuJ3BqqmVc9ESP\/koSXKN7kjHptMudF13lj9HxinzRPKn5n0cG7uqtLOo22lhEbv9vEIaSGuETUk9nRAkTP73gMz2EdrPv2ooN3njRPPjdG1zcaY2FMjI0cfzE\/kgXjyqwNfyRjPMUzFC9QvEbxDmfo+E85T5lYcV5y\/WLkp2u2jZ0ekDpOP6pwvFHssfHG8ed6YBR3DI+Nne6fOuLjtVuxLrbBthijN9YLvbHH\/2FaqbnOfkKG9AocS\/EkhWav+Jd6jbxB8TYa+wfe7j33GtbtbaPZG2Nsb8wh7BhmPzG90+aPXUTjL4soAn\/X+Msq3juc2Tf6P74DTjn\/URUqAAAAAElFTkSuQmCC","data:image\/png;base64,iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAYAAACqaXHeAAANXElEQVR42u2bBVQcSbfH572N+7cR4p51jUAWXdxdgkSQbOSTyPrG3XF3d4cYsnF3NEIcm2EGhxnkVs+rS2bPi8wK0P15nfM76XTfuvf+b1dVyzQ8jtt\/23\/bhymDePPD5g74PEJDwSxz1fQlJz1nLiuIp2TNXJqfh+A27ptmd8IDbdCWtyB4DmXgv6ho\/xEDFoapTLU7fuC9lWd+tna\/9+ibNEHnvsJmJvh6B4kt6SYp9wjJqJD2gNu4L4geQ5tv0gUdVh73Hr3reqpwqu2x\/QMWRSijz39+4Z8Fzpxglv3dx2sunF0VXdnofUlMUu4zJLJMStxvSsnOy1Ly\/TkprDslhbWFUlhVgLzYxn14DG3QFvsk32OI92UxWRX9vPEj6nOCec63vM\/DZvzzCf8kdNZEi+wdGltuFu3KayYJ5Qzxu8OQTReksLqAAbe8voF90Yc\/9YU+d+U1Ec2tt8umWB\/dg8X+xwtXCR\/5J4PU1ao\/XS\/ZQ4duDD1re64wZA1N3PUku6DPvdR3DC0ExlL+\/lrx23rpqzCHf4z4hWEfzVtemLs+WSCJLCZk5yWGrKSJupzgFoyBsTDmumS+eN6KwhzeopAP\/37Cv9wxYJRekr36plvl3pclxPM6IWvyCbgc\/\/uylsbE2J6XOojapptlo7VTlmBu3Io39Bk83iTzJ8eAJ6Kg20A2nWXA+Rj5h7LpHANBt4As8XskHGuU9SNvrs9gjha6mOEKFpn73CKrWvxvEfLnfIAVx\/45+EsBgD89IW4RVa0TrbIPYq6sn\/mJljkH1sTXSLxvEPLVCYDlR\/+5WHUSAHNbHVstUbDI3sfaSMB5NdYo86cV4ZUtHteAuB7rhmW57PPtz12w7VwXcTvedx+Ym8d1IMvDnrWMNcr4kWeb8la\/9Y\/STrK38q4QuV99IX5pDrt8dawTSvjdjFTWGsREuu2MhPTVnxvN0f1aN7HwrBCO0Ey26\/elTvG7ayWHLnWSVce7YGk2uzhlSuBqZSeKf6XVtnRLnTLa++x3FR1FmLPSd1fLeUoRH\/T5Jmf2soLcbfltZEN+NzhldbHOX3ObgZHKb+uPNffLN+a8Ja+VzHLKy+rTc8SfdFNXL4+oEm8+1U0cM7uAbRwyOmD\/qUYi\/ZX2lywR2vQrxpYz3WRZWFX72wbpbr2+t5+\/8XLxrnOdZEVOJzhkso99Wjt8n8uXW4CnIjFjE1ML9hmSfsVYTnNHDQvWX7rdq4coehnZvTZZRP56sosm0ckJS1LbwSqyCkqqWl6ZBW0dIF2XcI\/YJNSzEmddXhesoVom0Ie1P\/xIu2DjldKtZzqIIx2C9ukckSYBmzgRWPoVQdylKubKw0Ym40YtszzkDlhGVIJdcgsrcVADalm48WrRHxoF44yyv3OOrYM\/H6dnKa2DU2yTW8EqWgAm\/vfA0OsOGPuVg2VkDdgkNLIaB7U4xwpgnHH6N797uzvHufDc93kS4kDPkF0q99gmtYJ1fANY0dFgHVcPNkktrMdwSJcAaprrXHj6N68I+BrL8PAD0V9pxWxTJP9WoCb9gw9EAxTDv\/jVAky0PnrAJaEBltN5Y5Ms+bdiRWYHONOFVcH86B654vEN7AyngsK\/HRODXYqYdvr74JDSDlsKxcQlk9s4qAm1UY0Fct8246trpR9uP1iTKwHrJDHn2NKkfC6JiaAVXlz+Ool0XU4LpzFRm+IPN+\/xPgqe84Z+fBev7\/5YsiJDDFaJ7Zyy\/2w7qWzslr7e8u+3MZZxzZzFdaba9I88lAz4PEz9zVtfg\/TVFoHVxIEOFcuEdk5Ykd4GF5\/iw4\/8dut5G2MWJeIsvmOqGMwDqsloPTm3xgoWRz3to0X0stQGFvHss6OglTRJiPS3WtatOsY0UshJfMSOakONEy1yj7xRgMnWJxKWJTaDNRrHsYd1XCsk3m5nGEb6u21r1lNiGiHEfpyA2pYlNMEkq+NxbxbA9nj2iuTWHkPzWHZwTWuFcn5Xj\/SnQrF0e9p9si66RO4wILRCNqFPwDSqEftygiXVhhonWx\/PlFOAEwUuaWjYCmYsYBJZDw\/qupiOLiINKHhKtA9eBV3v+2AX\/gTkFeB+bRtjGFwFpjHN2J8TUJtLGi2A7cl8uQVwpgUwi2kFUxYwDOHD2uhy4hpyh2h7lIFBcDUYhQlh50mR3BGQfLWWMQoVYF\/OQG0rUn6lABNtjmYvTWyhhi1gEt1\/UKyu7yPQ9XsMhlSYcVQTGEfUQ9qtRrmrwY9pTwj2wb5cgdqc6Do3Sd4UmGR9LME+ls4\/LEAUO1BBFNH\/\/z9UCPf54jcK0NlNpCb+j2iBGtCOM1CbfUyD\/EVQwSzH0zq0Dsyjm8E4khssw\/kA5M0BcP1RI6MXUEkL0MRZbMQihuYQJoAJ8i6Do+mvvAZez4klrZJRRDP7hDfBphyh3PnvX1hJ9ANr0Y5TrKg2A89nZLR2upvcW2GNvRVim9hmMKTJso1BiBDirtbLnf+ukRVEP1iAdpyC2tT3PhDz5oeqyX0Y+mTD9ft28c1gQI3ZRi9IAMVV7W8UQNTSKdX2egT6oSK045QlVNvH66\/f5X0cMFvu4\/AUu7wC+zjZGQtjF6OgWsDF7vV24k4do+NXBQahjWjHGajJPrap5xL4qx9fjTfP2m\/sVwOmEU30jDSyR0gDrE8VyJ3\/u7OfEB3\/GrTjFLNIusD6VcNYk8zdv\/5K7LMwFcVNpUKraDpkQxpZQzdICCHn6+UWwDqwAnQCBGjHKahJ8adi0UClyMW\/+VJ0qn3eGevIBtqpAXSD2UHbnw+XHra8Mf\/5TR1STY\/HoBMoRDvOQC2oaeqSvFO\/+zPZaL2077QOPQHT8EbQCWpgBS2famgSv\/kC5OdSIaPp9fwVW+MwWVwWQS2aBx93jzFI+\/oP\/TAyx\/VssXU0rV5QPeiwgHNsjdwHIJ+850TTp6rHxiKiHk6UtTHPG7qkOgF8VuIiqAG1zFt5\/jZd\/Kbz\/kgbY5S+XefwM3o26PANrO8XWv51cDivTu78P3iskmj71sDBgibSKH5Rowv3GhgDv0rQDhD1OzbSo+HwE0J\/IN3eqy8+ZzmfvW0ZTquIIgL6zpe+tXCyRP4DUDdhpM2yt0TiTpDuz7hPFm+9DBruT2jhRP2Ki2DuFlTDTOczN3vOfm\/aSM2UVYt33GszCa0HTZpMX9HwqoLnIon0t1ppZQtj5XEVlPfcBnWPZ7RofOzbbzB3pW13W0dqJrr26YNnBcvj2fo+9BodRM+kn6hPqHk8h4radrkjoFXSLT2U\/YAobb0AKgfvU\/GVVLwQ+\/UbzFmPLr5UQ0afvxrDLzBnOZ8pMgkSgra\/EJPrNWruz2BfVgV5WXh7B0gjTz9ldHdfgC923QK1w4\/pSKlBe1bAXDHn2c5nSmWfyPS9DVVPXPLxhlt1piF0WFHnGj69AQtQBYt33YYfYktI5tVqxvf4I6K58zwdmtdA5UAFPV4JGt4CtGUFzBFz\/WjDLf5Q5QRrXn8bfmpG14Pv5v9Q3GASIqIVrgN1n15AxakcfgJKtAiLtlwGxR3XQfnAA1A98gzUvGrQhjUwN8xx\/g9FTSN107\/H3HlsNPzokL4v2Ke4tawdq6tBg6l59wIvWgQ6ElSOPKf\/VoKqZy3uZxUUj7kpbiltx1zxL1VY\/1R2pH7q3gU\/FDeah+JQqwNVKuyfAcwFc1rwY3HzaIOM\/fSSN4zHQftfnsKy4SM14zd9suFmnUVIHej50yJ4Cv6hYA4WwQKgOQlGaib8xJu8ahjmyrp4ygDKIJ7CJ8MHKQY7zlr+c5GRTzWYBglBHYe4h3yU3fmkP\/yaXw0a0yxYCEbeVTB7xamyIYsDnTA3zBFzZbMI\/0N5izKQMpgylDKC9+FmxfGmWUcXby1tt6ajwQBHgwcfBcMXR2oJF6BvjIGxMKbSlrK2caaZubx3flyEOclyGyzL9S3MnbMCUEbxxmpNGaocvG6GU36JzoHHxDZcCIZ+AqJMk118mF3QJ\/rGGNr7H5KZTvnFQ5WD\/sYbpzsZc+GmAHKmAGUIZdgvRaC8zZvqqDhcLdBjhsPxEq19D7rtQgXEPEhAtL3xzNUQpcN9A\/uiD\/SFPrX2VpAZS\/MqRqgHe\/OmLVmEsV8SPwxzkz8FuCvCcMpoigJlBm+iwZfDFQ+6K5in31jw7Y0mI\/enxD6sjtiE1hGTAAHR8+ETTc9aoupOC0MFLj70AtzGfXgMbdAW+2Bf9DH\/mxtNCuapN4YrHTmCMTCWLCbGHt4H8awWYuhLRZhOmccbNnshb9oy55EqnlFjDeNvvLfyVKXy1uJOnf0VxMz7GbEJrCb2oXziFCFAerZxHx5DG7R9d+Wp52MNE66PVPaKQF\/oE31jjJfED2VBOCtrwy\/FGEOZQJlKmU15jzdwzOe8cXomvOnLvhr6+bYDo9R9k0ZrhheM0Yk6P0Y35noPOtHncd8odZ+kIZ9u34+22Af7og+Zr6ky32N+Ec3OXGe\/IANkCY6ijKUoyJKfLhMyl\/IuCnuNd2XHZstsp8r6jpX5Qp8DuBfMRWFk0+alq8jwlxZRBLeHv7SKy4Yz90L\/D1wmRcCSWFt6AAAAAElFTkSuQmCC"];
    $impl.ButtonModalResult = [6,7,1,2,3,4,5,8,9,10,0,11];
    rtl.createClass($impl,"TMessageDialog",pas.Forms.TWForm,function () {
      this.CControlsSpacing = 2;
      this.CMinDialogHeight = 150;
      this.CMinDialogWidth = 300;
      this.CMinButtonHeight = 25;
      this.CMinButtonWidth = 100;
      this.CMinImageHeight = 70;
      this.CMinImageWidth = 70;
      this.CTitleHeight = 24;
      this.$init = function () {
        pas.Forms.TWForm.$init.call(this);
        this.FButtons = {};
        this.FDefaultButton = 0;
        this.FDialogType = 0;
        this.FMessage = "";
        this.FButtonPanel = null;
        this.FInfoImage = null;
        this.fMessagePanel = null;
        this.FMessageText = null;
        this.fTitlePanel = null;
        this.fTitleText = null;
      };
      this.$final = function () {
        this.FButtons = undefined;
        this.FButtonPanel = undefined;
        this.FInfoImage = undefined;
        this.fMessagePanel = undefined;
        this.FMessageText = undefined;
        this.fTitlePanel = undefined;
        this.fTitleText = undefined;
        pas.Forms.TWForm.$final.call(this);
      };
      this.PrepareButtons = function () {
        var VMsgDlgBtn = 0;
        var VButton = null;
        var VButtonCount = 0;
        var VButtonHeight = 0;
        var VButtonWidth = 0;
        var VFormWidth = 0;
        var VSize = pas.Types.TSize.$new();
        var buttonofs = 0;
        VButtonCount = 0;
        buttonofs = 0;
        VButtonHeight = 25;
        VButtonWidth = 100;
        this.BeginUpdate();
        try {
          for (var $l = $mod.TMsgDlgBtn.mbYes, $end = $mod.TMsgDlgBtn.mbClose; $l <= $end; $l++) {
            VMsgDlgBtn = $l;
            if (VMsgDlgBtn in this.FButtons) {
              VButtonCount += 1;
              VSize.$assign(pas.Graphics.JSMeasureText($impl.ButtonCaption[VMsgDlgBtn],this.FFont.FName,this.FFont.FSize,0));
              if (VSize.cy > VButtonHeight) {
                VButtonHeight = VSize.cy;
              };
              if (VSize.cx > VButtonWidth) {
                VButtonWidth = VSize.cx;
              };
            };
          };
          for (var $l1 = $mod.TMsgDlgBtn.mbYes, $end1 = $mod.TMsgDlgBtn.mbClose; $l1 <= $end1; $l1++) {
            VMsgDlgBtn = $l1;
            if (VMsgDlgBtn in this.FButtons) {
              VButton = pas.WebCtrls.TWButton.$create("Create$1",[this.FButtonPanel]);
              VButton.BeginUpdate();
              try {
                VButton.SetParent(this.FButtonPanel);
                VButton.FBorderSpacing.SetAround(2);
                VButton.SetBounds(buttonofs,0,VButtonWidth,VButtonHeight);
                VButton.FModalResult = $impl.ButtonModalResult[VMsgDlgBtn];
                VButton.SetText($impl.ButtonCaption[VMsgDlgBtn]);
                VButton.SetAlign(4);
              } finally {
                VButton.EndUpdate();
              };
              if (VMsgDlgBtn === this.FDefaultButton) {
                this.SetActiveControl(VButton);
              };
            };
            buttonofs = buttonofs + VButtonWidth;
          };
          this.FButtonPanel.SetHeight(VButtonHeight + (2 * 2));
          VFormWidth = ((VButtonWidth + (2 * 2)) * VButtonCount) + (2 * 2);
          if (VFormWidth < 300) {
            VFormWidth = 300;
          };
          this.SetWidth(VFormWidth);
        } finally {
          this.EndUpdate();
        };
      };
      this.PrepareImage = function () {
        this.FInfoImage.SetURL($impl.DialogIcon[this.FDialogType]);
      };
      this.PrepareText = function () {
        this.FMessageText.SetText(this.FMessage);
      };
      this.PrepareTitle = function () {
        this.SetText(pas.Controls.IfThen$3(this.GetText() !== "",this.GetText(),$impl.DialogCaption[this.FDialogType]));
        this.fTitleText.SetText(this.GetText());
      };
      this.PrepareLayout = function () {
        this.PrepareTitle();
        this.PrepareText();
        this.PrepareImage();
        this.PrepareButtons();
      };
      this.KeyDown = function (Key, Shift) {
        pas.Controls.TWinControl.KeyDown.call(this,Key,rtl.refSet(Shift));
        var $tmp = Key.get();
        if ($tmp === 27) {
          this.SetModalResult(2);
          this.Close();
        };
      };
      this.Show = function () {
        var ownForm = null;
        var curHeight = 0;
        pas.Forms.TCustomForm.Show.call(this);
        if (!pas.Forms.TWForm.isPrototypeOf(this.FOwner)) return;
        ownForm = this.FOwner;
        this.BeginUpdate();
        try {
          curHeight = this.FMessageText.FHandleElement.scrollHeight + 25 + (2 * 8) + 24;
          if ((curHeight - 70) > ownForm.FHeight) curHeight = ownForm.FHeight - 70;
          if (curHeight < 150) curHeight = 150;
          this.SetHeight(curHeight);
          this.fMessagePanel.SetHeight(this.FMessageText.FHandleElement.scrollHeight);
          this.SetTop(Math.round((ownForm.FHeight / 2) - (this.FHeight / 2)));
        } finally {
          this.EndUpdate();
        };
      };
      this.Create$1 = function (AOwner) {
        pas.Forms.TCustomForm.CreateNew.call(this,AOwner,1);
        this.BeginUpdate();
        try {
          this.FKeyPreview = true;
          this.SetBounds(0,0,300,150);
          this.FButtonPanel = pas.WebCtrls.TWPanel.$create("Create$1",[this]);
          this.FButtonPanel.BeginUpdate();
          try {
            this.FButtonPanel.SetParent(this);
            this.FButtonPanel.FBorderSpacing.SetAround(2);
            this.FButtonPanel.SetBevelOuter(0);
            this.FButtonPanel.SetBounds(0,0,300,25);
            this.FButtonPanel.SetAlign(2);
          } finally {
            this.FButtonPanel.EndUpdate();
          };
          this.fTitlePanel = pas.WebCtrls.TWPanel.$create("Create$1",[this]);
          this.fTitlePanel.BeginUpdate();
          try {
            this.fTitlePanel.SetParent(this);
            this.fTitlePanel.FBorderSpacing.SetAround(2);
            this.fTitlePanel.SetHeight(24);
            this.fTitlePanel.SetBevelOuter(0);
            this.fTitlePanel.SetAlign(1);
            this.fTitlePanel.SetColor(15780518);
          } finally {
            this.fTitlePanel.EndUpdate();
          };
          this.fTitleText = pas.WebCtrls.TWLabel.$create("Create$1",[this.fTitlePanel]);
          this.fTitleText.BeginUpdate();
          try {
            this.fTitleText.SetParent(this.fTitlePanel);
            this.fTitleText.FBorderSpacing.SetAround(2);
            this.fTitleText.SetAlign(5);
            this.fTitleText.SetAlignment(2);
          } finally {
            this.fTitleText.EndUpdate();
          };
          this.FInfoImage = pas.WebCtrls.TWImage.$create("Create$1",[this]);
          this.FInfoImage.BeginUpdate();
          try {
            this.FInfoImage.SetParent(this);
            this.FInfoImage.FBorderSpacing.SetAround(2);
            this.FInfoImage.SetBounds(0,0,70,70);
            this.FInfoImage.SetCenter(true);
            this.FInfoImage.SetAlign(3);
          } finally {
            this.FInfoImage.EndUpdate();
          };
          this.fMessagePanel = pas.WebCtrls.TWPanel.$create("Create$1",[this]);
          this.fMessagePanel.BeginUpdate();
          try {
            this.fMessagePanel.SetParent(this);
            this.fMessagePanel.FBorderSpacing.SetAround(2);
            this.fMessagePanel.SetBevelOuter(0);
            this.fMessagePanel.SetAlign(5);
          } finally {
            this.fMessagePanel.EndUpdate();
          };
          this.FMessageText = pas.WebCtrls.TWLabel.$create("Create$1",[this.fMessagePanel]);
          this.FMessageText.BeginUpdate();
          try {
            this.FMessageText.SetParent(this.fMessagePanel);
            this.FMessageText.FBorderSpacing.SetAround(2);
            this.FMessageText.SetWordWrap(true);
            this.FMessageText.SetAlign(5);
          } finally {
            this.FMessageText.EndUpdate();
          };
        } finally {
          this.EndUpdate();
        };
        return this;
      };
      rtl.addIntf(this,pas.System.IUnknown);
      var $r = this.$rtti;
      $r.addProperty("Buttons",0,$mod.$rtti["TMsgDlgButtons"],"FButtons","FButtons");
      $r.addProperty("DefaultButton",0,$mod.$rtti["TMsgDlgBtn"],"FDefaultButton","FDefaultButton");
      $r.addProperty("DialogType",0,$mod.$rtti["TMsgDlgType"],"FDialogType","FDialogType");
      $r.addProperty("Message",0,rtl.string,"FMessage","FMessage");
    });
    $impl.ModalDefaultButton = function (AButtons) {
      var Result = 0;
      if (0 in AButtons) {
        Result = 0;
      } else if (2 in AButtons) {
        Result = 2;
      } else if (9 in AButtons) {
        Result = 9;
      } else if (7 in AButtons) {
        Result = 7;
      } else if (5 in AButtons) {
        Result = 5;
      } else if (10 in AButtons) {
        Result = 10;
      } else if (3 in AButtons) {
        Result = 3;
      } else if (1 in AButtons) {
        Result = 1;
      } else if (8 in AButtons) {
        Result = 8;
      } else if (4 in AButtons) {
        Result = 4;
      } else if (6 in AButtons) {
        Result = 6;
      } else if (11 in AButtons) {
        Result = 11;
      } else {
        Result = 2;
      };
      return Result;
    };
  };
},[]);
rtl.module("Unit1",["System","JS","Classes","SysUtils","Graphics","Controls","Forms","Dialogs","WebCtrls"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass(this,"TWForm2",pas.Forms.TWForm,function () {
    this.$init = function () {
      pas.Forms.TWForm.$init.call(this);
      this.WButton1 = null;
    };
    this.$final = function () {
      this.WButton1 = undefined;
      pas.Forms.TWForm.$final.call(this);
    };
    this.WButton1Click = function (Sender) {
      pas.main.WForm1.Show();
    };
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addField("WButton1",pas.WebCtrls.$rtti["TWButton"]);
    $r.addMethod("WButton1Click",0,[["Sender",pas.System.$rtti["TObject"]]]);
  });
  this.WForm2 = null;
},["main"]);
rtl.module("Unit2",["System","JS","Classes","SysUtils","Graphics","Controls","Forms","Dialogs","WebCtrls"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass(this,"TWForm3",pas.Forms.TWForm,function () {
    rtl.addIntf(this,pas.System.IUnknown);
  });
  this.WForm3 = null;
});
rtl.module("main",["System","JS","Web","Classes","SysUtils","Graphics","Controls","Forms","WebCtrls","Menus","Unit1","Unit2","Dialogs","ComCtrls","StdCtrls"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass(this,"TWForm1",pas.Forms.TWForm,function () {
    this.$init = function () {
      pas.Forms.TWForm.$init.call(this);
      this.MenuItem1 = null;
      this.MenuItem2 = null;
      this.MenuItem3 = null;
      this.MenuItem4 = null;
      this.MenuItem5 = null;
      this.MenuItem6 = null;
      this.TabSheet1 = null;
      this.TabSheet2 = null;
      this.TabSheet3 = null;
      this.WButton1 = null;
      this.WButton2 = null;
      this.WButton3 = null;
      this.WButton4 = null;
      this.lblUserName = null;
      this.WButton5 = null;
      this.WButton6 = null;
      this.WButton7 = null;
      this.WButton8 = null;
      this.WButton9 = null;
      this.WCheckbox1 = null;
      this.WCheckbox2 = null;
      this.WCheckbox3 = null;
      this.WComboBox1 = null;
      this.WDateEditBox1 = null;
      this.WEdit1 = null;
      this.WFileButton1 = null;
      this.WFloatEdit1 = null;
      this.WImage1 = null;
      this.WIntegerEdit1 = null;
      this.WLabel1 = null;
      this.WLabel2 = null;
      this.WLabel3 = null;
      this.WListBox1 = null;
      this.WMemo1 = null;
      this.WPageControl1 = null;
      this.WPanel1 = null;
      this.WPanel10 = null;
      this.WPanel11 = null;
      this.WPanel2 = null;
      this.WPanel3 = null;
      this.WPanel4 = null;
      this.WPanel5 = null;
      this.WPanel6 = null;
      this.WPanel7 = null;
      this.WPanel8 = null;
      this.WPanel9 = null;
      this.WPopupMenu1 = null;
      this.WRadioButton1 = null;
      this.WRadioButton2 = null;
      this.WSplitter1 = null;
      this.WSplitter2 = null;
      this.WTimeEditBox1 = null;
      this.WTimer1 = null;
      this.FDrag = false;
    };
    this.$final = function () {
      this.MenuItem1 = undefined;
      this.MenuItem2 = undefined;
      this.MenuItem3 = undefined;
      this.MenuItem4 = undefined;
      this.MenuItem5 = undefined;
      this.MenuItem6 = undefined;
      this.TabSheet1 = undefined;
      this.TabSheet2 = undefined;
      this.TabSheet3 = undefined;
      this.WButton1 = undefined;
      this.WButton2 = undefined;
      this.WButton3 = undefined;
      this.WButton4 = undefined;
      this.lblUserName = undefined;
      this.WButton5 = undefined;
      this.WButton6 = undefined;
      this.WButton7 = undefined;
      this.WButton8 = undefined;
      this.WButton9 = undefined;
      this.WCheckbox1 = undefined;
      this.WCheckbox2 = undefined;
      this.WCheckbox3 = undefined;
      this.WComboBox1 = undefined;
      this.WDateEditBox1 = undefined;
      this.WEdit1 = undefined;
      this.WFileButton1 = undefined;
      this.WFloatEdit1 = undefined;
      this.WImage1 = undefined;
      this.WIntegerEdit1 = undefined;
      this.WLabel1 = undefined;
      this.WLabel2 = undefined;
      this.WLabel3 = undefined;
      this.WListBox1 = undefined;
      this.WMemo1 = undefined;
      this.WPageControl1 = undefined;
      this.WPanel1 = undefined;
      this.WPanel10 = undefined;
      this.WPanel11 = undefined;
      this.WPanel2 = undefined;
      this.WPanel3 = undefined;
      this.WPanel4 = undefined;
      this.WPanel5 = undefined;
      this.WPanel6 = undefined;
      this.WPanel7 = undefined;
      this.WPanel8 = undefined;
      this.WPanel9 = undefined;
      this.WPopupMenu1 = undefined;
      this.WRadioButton1 = undefined;
      this.WRadioButton2 = undefined;
      this.WSplitter1 = undefined;
      this.WSplitter2 = undefined;
      this.WTimeEditBox1 = undefined;
      this.WTimer1 = undefined;
      pas.Forms.TWForm.$final.call(this);
    };
    this.FormCreate = function (Sender) {
      document.onclick = rtl.createSafeCallback(this,"test");
    };
    this.MenuItem2Click = function (Sender) {
      pas.Dialogs.ShowMessage$1("It's a Show Message Dialog");
    };
    this.WButton1Click = function (Sender) {
      if (this.WPanel5.FColor === 32768) {
        this.WPanel5.SetColor(255)}
       else this.WPanel5.SetColor(32768);
    };
    this.WButton2Click = function (Sender) {
      pas.System.Writeln(pas.Forms.Application().FActiveForm.GetText());
      pas.Unit1.WForm2.Show();
      pas.System.Writeln(pas.Forms.Application().FActiveForm.GetText());
    };
    this.WButton3Click = function (Sender) {
      if (this.WPanel9.FAlpha === 100) {
        this.WPanel9.SetAlpha(255)}
       else this.WPanel9.SetAlpha(100);
    };
    this.WButton5Click = function (Sender) {
      this.lblUserName.SetText("DEMO");
    };
    this.WListBox1Click = function (Sender) {
    };
    this.WListBox1SelectionChange = function (Sender, User) {
      this.WLabel3.SetText(this.WListBox1.FItems.Get(this.WListBox1.FItemIndex));
    };
    this.WPanel5Click = function (Sender) {
      pas.System.Writeln(this.WPanel5.FLeft," = ",this.WPanel5.FTop);
    };
    this.WPanel8Click = function (Sender) {
      pas.System.Writeln("Click2");
    };
    this.WPanel9Click = function (Sender) {
      pas.System.Writeln("Click3");
    };
    this.WPanel9MouseDown = function (Sender, Button, Shift, X, Y) {
      pas.System.Writeln("Down");
      this.FDrag = true;
    };
    this.WPanel9MouseMove = function (Sender, Shift, X, Y) {
      if (this.FDrag) {
        pas.System.Writeln(X," - ",Y);
      };
      pas.System.Writeln("Move");
    };
    this.WPanel9MouseUp = function (Sender, Button, Shift, X, Y) {
      pas.System.Writeln("Up");
      this.FDrag = false;
    };
    this.WTimer1Timer = function (Sender) {
      this.WLabel1.SetText(pas.SysUtils.DateTimeToStr(pas.SysUtils.Now(),false));
    };
    this.test = function (aEvent) {
      var Result = false;
      pas.System.Writeln("Global");
      return Result;
    };
    rtl.addIntf(this,pas.System.IUnknown);
    var $r = this.$rtti;
    $r.addField("MenuItem1",pas.Menus.$rtti["TMenuItem"]);
    $r.addField("MenuItem2",pas.Menus.$rtti["TMenuItem"]);
    $r.addField("MenuItem3",pas.Menus.$rtti["TMenuItem"]);
    $r.addField("MenuItem4",pas.Menus.$rtti["TMenuItem"]);
    $r.addField("MenuItem5",pas.Menus.$rtti["TMenuItem"]);
    $r.addField("MenuItem6",pas.Menus.$rtti["TMenuItem"]);
    $r.addField("TabSheet1",pas.ComCtrls.$rtti["TTabSheet"]);
    $r.addField("TabSheet2",pas.ComCtrls.$rtti["TTabSheet"]);
    $r.addField("TabSheet3",pas.ComCtrls.$rtti["TTabSheet"]);
    $r.addField("WButton1",pas.WebCtrls.$rtti["TWButton"]);
    $r.addField("WButton2",pas.WebCtrls.$rtti["TWButton"]);
    $r.addField("WButton3",pas.WebCtrls.$rtti["TWButton"]);
    $r.addField("WButton4",pas.WebCtrls.$rtti["TWButton"]);
    $r.addField("lblUserName",pas.WebCtrls.$rtti["TWLabel"]);
    $r.addField("WButton5",pas.WebCtrls.$rtti["TWButton"]);
    $r.addField("WButton6",pas.WebCtrls.$rtti["TWButton"]);
    $r.addField("WButton7",pas.WebCtrls.$rtti["TWButton"]);
    $r.addField("WButton8",pas.WebCtrls.$rtti["TWButton"]);
    $r.addField("WButton9",pas.WebCtrls.$rtti["TWButton"]);
    $r.addField("WCheckbox1",pas.WebCtrls.$rtti["TWCheckbox"]);
    $r.addField("WCheckbox2",pas.WebCtrls.$rtti["TWCheckbox"]);
    $r.addField("WCheckbox3",pas.WebCtrls.$rtti["TWCheckbox"]);
    $r.addField("WComboBox1",pas.WebCtrls.$rtti["TWComboBox"]);
    $r.addField("WDateEditBox1",pas.WebCtrls.$rtti["TWDateEditBox"]);
    $r.addField("WEdit1",pas.WebCtrls.$rtti["TWEdit"]);
    $r.addField("WFileButton1",pas.WebCtrls.$rtti["TWFileButton"]);
    $r.addField("WFloatEdit1",pas.WebCtrls.$rtti["TWFloatEdit"]);
    $r.addField("WImage1",pas.WebCtrls.$rtti["TWImage"]);
    $r.addField("WIntegerEdit1",pas.WebCtrls.$rtti["TWIntegerEdit"]);
    $r.addField("WLabel1",pas.WebCtrls.$rtti["TWLabel"]);
    $r.addField("WLabel2",pas.WebCtrls.$rtti["TWLabel"]);
    $r.addField("WLabel3",pas.WebCtrls.$rtti["TWLabel"]);
    $r.addField("WListBox1",pas.WebCtrls.$rtti["TWListBox"]);
    $r.addField("WMemo1",pas.WebCtrls.$rtti["TWMemo"]);
    $r.addField("WPageControl1",pas.WebCtrls.$rtti["TWPageControl"]);
    $r.addField("WPanel1",pas.WebCtrls.$rtti["TWPanel"]);
    $r.addField("WPanel10",pas.WebCtrls.$rtti["TWPanel"]);
    $r.addField("WPanel11",pas.WebCtrls.$rtti["TWPanel"]);
    $r.addField("WPanel2",pas.WebCtrls.$rtti["TWPanel"]);
    $r.addField("WPanel3",pas.WebCtrls.$rtti["TWPanel"]);
    $r.addField("WPanel4",pas.WebCtrls.$rtti["TWPanel"]);
    $r.addField("WPanel5",pas.WebCtrls.$rtti["TWPanel"]);
    $r.addField("WPanel6",pas.WebCtrls.$rtti["TWPanel"]);
    $r.addField("WPanel7",pas.WebCtrls.$rtti["TWPanel"]);
    $r.addField("WPanel8",pas.WebCtrls.$rtti["TWPanel"]);
    $r.addField("WPanel9",pas.WebCtrls.$rtti["TWPanel"]);
    $r.addField("WPopupMenu1",pas.WebCtrls.$rtti["TWPopupMenu"]);
    $r.addField("WRadioButton1",pas.WebCtrls.$rtti["TWRadioButton"]);
    $r.addField("WRadioButton2",pas.WebCtrls.$rtti["TWRadioButton"]);
    $r.addField("WSplitter1",pas.WebCtrls.$rtti["TWSplitter"]);
    $r.addField("WSplitter2",pas.WebCtrls.$rtti["TWSplitter"]);
    $r.addField("WTimeEditBox1",pas.WebCtrls.$rtti["TWTimeEditBox"]);
    $r.addField("WTimer1",pas.WebCtrls.$rtti["TWTimer"]);
    $r.addMethod("FormCreate",0,[["Sender",pas.System.$rtti["TObject"]]]);
    $r.addMethod("MenuItem2Click",0,[["Sender",pas.System.$rtti["TObject"]]]);
    $r.addMethod("WButton1Click",0,[["Sender",pas.System.$rtti["TObject"]]]);
    $r.addMethod("WButton2Click",0,[["Sender",pas.System.$rtti["TObject"]]]);
    $r.addMethod("WButton3Click",0,[["Sender",pas.System.$rtti["TObject"]]]);
    $r.addMethod("WButton5Click",0,[["Sender",pas.System.$rtti["TObject"]]]);
    $r.addMethod("WListBox1Click",0,[["Sender",pas.System.$rtti["TObject"]]]);
    $r.addMethod("WListBox1SelectionChange",0,[["Sender",pas.System.$rtti["TObject"]],["User",rtl.boolean]]);
    $r.addMethod("WPanel5Click",0,[["Sender",pas.System.$rtti["TObject"]]]);
    $r.addMethod("WPanel8Click",0,[["Sender",pas.System.$rtti["TObject"]]]);
    $r.addMethod("WPanel9Click",0,[["Sender",pas.System.$rtti["TObject"]]]);
    $r.addMethod("WPanel9MouseDown",0,[["Sender",pas.System.$rtti["TObject"]],["Button",pas.Controls.$rtti["TMouseButton"]],["Shift",pas.Controls.$rtti["TShiftState"]],["X",rtl.nativeint],["Y",rtl.nativeint]]);
    $r.addMethod("WPanel9MouseMove",0,[["Sender",pas.System.$rtti["TObject"]],["Shift",pas.Controls.$rtti["TShiftState"]],["X",rtl.nativeint],["Y",rtl.nativeint]]);
    $r.addMethod("WPanel9MouseUp",0,[["Sender",pas.System.$rtti["TObject"]],["Button",pas.Controls.$rtti["TMouseButton"]],["Shift",pas.Controls.$rtti["TShiftState"]],["X",rtl.nativeint],["Y",rtl.nativeint]]);
    $r.addMethod("WTimer1Timer",0,[["Sender",pas.System.$rtti["TObject"]]]);
  });
  this.WForm1 = null;
});
rtl.module("program",["System","Forms","main","Unit1","Unit2"],function () {
  "use strict";
  var $mod = this;
  $mod.$main = function () {
    pas.Forms.Application().Initialize();
    pas.Forms.Application().CreateForm(pas.main.TWForm1,{p: pas.main, get: function () {
        return this.p.WForm1;
      }, set: function (v) {
        this.p.WForm1 = v;
      }});
    pas.Forms.Application().CreateForm(pas.Unit1.TWForm2,{p: pas.Unit1, get: function () {
        return this.p.WForm2;
      }, set: function (v) {
        this.p.WForm2 = v;
      }});
    pas.Forms.Application().CreateForm(pas.Unit2.TWForm3,{p: pas.Unit2, get: function () {
        return this.p.WForm3;
      }, set: function (v) {
        this.p.WForm3 = v;
      }});
    pas.Forms.Application().Run();
  };
});
//# sourceMappingURL=demo.js.map
