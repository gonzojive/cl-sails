var Sauve = { util : {  } };

var sails = {  };

var sailController =
    ensureClass
    (sailController,
     { name : 'sail-controller', 
       directSuperclasses : null, 
       slotDefinitions : null, 
       documentation : 'Replaces Sail.Controller' });

var addSubsail = ensureGeneric(addSubsail, { name : 'add-subsail' });

var addSubsail = ensureGeneric(addSubsail, { name : 'add-subsail' });
ensureMethod
(addSubsail, [ sailController, sailController ],
 function (parentSail, subsail, groupName) {
   groupName = groupName || 'children';
   var group =
       parentSail.sub[groupName] = parentSail.sub[groupName] || [  ];
   var insertionElem = parentSail.insertionElem;
   var insertIndex = group.indexOf(subsail);
   var htmlWriter =
       group.length == 1 ?
         function (rawHtml) {
           insertByGroupName(sail, groupName, rawHtml);
         }
         :
         (insertIndex == 0 ?
           function (rawHtml) {
             new Insertion.Before
               (sailFirstNode(sailView(group[1])), rawHtml);
           }
           :
           function (rawHtml) {
             new Insertion.After
               (sailLastNode(sailView(group[insertIndex - 1])), rawHtml);
           });
   subsail.parentSail = parentSail;
   group.push(subsail);
   group.sort(group.sorter);
   manifestSail(subsail, htmlWriter);
 });

var manifestSail = ensureGeneric(manifestSail, { name : 'manifest-sail' });
ensureMethod
(manifestSail, [ sailController, null ],
 function (sail, writer) {
   renderHtml(sail.view, writer);
 });

var sailView =
    ensureClass
    (sailView,
     { name : 'sail-view', 
       directSuperclasses : null, 
       slotDefinitions : null, 
       documentation : 'The sail object responsible for rendering information on the page.' });

var htmlSailView =
    ensureClass
    (htmlSailView,
     { name : 'html-sail-view', 
       directSuperclasses : [ sailView ], 
       slotDefinitions : null });

var initializeInstance =
    ensureGeneric(initializeInstance, { name : 'initialize-instance' });
ensureMethod
(initializeInstance, [ htmlSailView ],
 function (view) {
   this.callFollowingMethod();
   view.dom = {  };
   view.domInfo = {  };
 });

var renderHtml = ensureGeneric(renderHtml, { name : 'render-html' });
ensureMethod
(renderHtml, [ htmlSailView, null, null ],
 function (view, writer, doc) {
   writer(generateHtml(view));
 });

var renderHtml = ensureGeneric(renderHtml, { name : 'render-html' });
ensureMethod
(renderHtml, [ htmlSailView, null ],
 function (view, writer, doc) {
   doc = doc || document;
   view.domInfo = locateDomNodes(doc);
 },
 'after');

var locateDomNodes =
    ensureGeneric(locateDomNodes, { name : 'locate-dom-nodes' });
ensureMethod
(locateDomNodes, [ htmlSailView, null ],
 function (view, doc) {
   var rv = {  };
   for (var fieldName in view.domInfo) {
     rv[fieldName] = doc.getElementById(view.domInfo[fieldName].id);
   };
   return rv;
 });

var sailsIdCounter = 0;

function genFieldId(name) {
  name = name || 'anon';
  var num = ++sailsIdCounter;
  return name + num;
};

var defField = ensureGeneric(defField, { name : 'def-field' });
ensureMethod
(defField, [ null, null ],
 function (view, name, &keys, first, last, id) {
   var info = view.domInfo[name] = {  };
   info.id = id || genFieldId(name);
   if (first) {
     info.first = true;
   };
   if (last) {
     info.first = true;
   };
   return info.id;
 });

