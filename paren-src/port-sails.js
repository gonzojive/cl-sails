/**
   Sails is a simple alternative interactive language sweetener.  It is a way to generate
   dynamic components for web pages, often called 'widgets' in other systems.  Sails
   uses the Model-View-Controller software design pattern to describe components.
   
   MVC is an elusive term.  Here are some helpful pages:
      http://www.enode.com/x/markup/tutorial/mvc.html  -- has a neat diagram
      http://ootips.org/mvc-pattern.html -- consolidation of newsgroup posts
      http://c2.com/cgi/wiki?ModelViewController -- WikiWikiWeb discussion
   
   The provided sails infrastructure is lightweight, only providing common methods for manipulating
   HTML components.  These common features are the abilities to:
      *insert raw HTML from a component into a document
      *include other components within a component
      *assign names to significant HTML elements within the component (e.g. title, authorlink)

*/

Suave = { util : {}}
sails = new Object();
//augments the first argument's properties with the other objects'
//subsequent arguments' properites overwrite former arguments' properties
//this can be shortcut for prototypal (ahem, all) inheritance schemes that batch inherit
Suave.util.mixInto = function(obj /*...*/)
{
   for (var i=1; i < arguments.length; i++)
   {
      for (var key in arguments[i])
         obj[key] = arguments[i][key]
   }
}

var Sail = {}


// Note: it is not necessary to implement a Controller, or to use this class at all
// However, this class provides functionality that you will probably end up needing
// if you implement any complex component system (with subcomponents, event listeners,
// and complicated data in the 'model'
Sail.Controller = function(view, model)
{
   //this.sub and this.parent both reference other Controller objects
   //this.sub maps subcomponent group names to arrays of subcomponents
   this.sub = {__inlines : new Array() }
   //this.parent holds a reference to a parent controller
   this.parent = null;
   //the view and model associated with this controller
   this.view = view
   if (this.view)
   {
      var controller = this
      this.view.performInclusion = function(subname, sub_constructor)
      {
	 controller.sub[subname] = controller.sub[subname] || sub_constructor()
	 //Log.msg(controller);
	 var markup = controller.sub[subname].view.generateHTML()
	 controller.sub.__inlines.push(controller.sub[subname])
	 return markup;
      }
   }
}

Suave.util.mixInto(
   Sail.Controller.prototype,
   {
      addSubsail : function(sub_sail, groupname)
      {
	 groupname = groupname || "children"
	 if (!this.sub[groupname])
	    this.sub[groupname] = this.formSubGroup(groupname)
	 this.sub[groupname].insertSail(sub_sail)
      },
      insertFromGroupname : function(groupname)
      {
	 var opts = this.view.dom_opts[groupname]
	 var ins_loc = opts && opts.insertionLocation;
	 ins_loc = ins_loc || "bottom";
	 ins_loc = ins_loc[0].toUpperCase() + ins_loc.substr(1)
	 return Insertion[ins_loc]	 
      },
      formSubGroup : function(groupname, insert_func, sort_func, remove_func)
      {
	 var sail = this;
	 var group = new Array();
	 group.insertion_elem = this.view.dom[groupname] || this.view.dom.root
	 var orig_elem_inserter = this.insertFromGroupname(groupname)
	 group.sailsSorter = sort_func;
	 group.insertSail = insert_func || function(comp)
	 {
	    comp.parentSail=sail;
	    var insertion_elem = this.insertion_elem;
	    //1. push component onto element group
	    //2. sort sails
	    //3. write the new sail's html
	    //      -after the previous sail if one exists
	    //      -before the next sail if one exists
	    //      -else at the bottom of the insertion function
	    this.push(comp)
	    if (this.sailsSorter)
	       this.sort(this.sailsSorter);
	    var insert_index = this.indexOf(comp)
	    //3.
	    var html_writer; var self = this;
	    if (this.length == 1)
	       html_writer = function(html) { new orig_elem_inserter(insertion_elem, html); }
	    else if (insert_index == 0)
	       html_writer = function(html) { new Insertion.Before(self[1].view.getFirstNode(), html) }
	    else
	       html_writer = function(html) { new Insertion.After(self[insert_index - 1].view.getLastNode(), html) }
	    comp.create(html_writer)
	 }
	 //TODO remover is not refined
	 group.removeSail = remove_func || function(comp) {  comp.destroy(); }
	 return group
      },
      create : function(writer)
      {
	 this.view.renderHTML(writer);
	 this.sub.__inlines.each(function(sub_controller) { sub_controller.view.afterWrite()})
      },
      destroy : function() {},
      setParent : function(parent) { this.parentSail = parent;},
      getParent : function() { return this.parentSail; }
   })

Sail.View = function(model)
{
   //Log.msg("constructor of Sail.View called")
   //this.model connects the view to the model being renderred
   this.model = model
}
Sail.View.prototype = {
   //YouOverrideThisFunction
   paint : function(){}
}


//Constructor: YouCallThisFunction, YouOverrideThisFunction
var HTMLSail = {}
HTMLSail.View = function(model)
{
   Sail.View.call(this)
   //this.dom maps field names to DOM nodes
   this.dom = new Object();
   //this.dom_ids maps field names to DOM ids.  this is used internally
   this.dom_ids = new Object();
   this.dom_opts = {};
}
HTMLSail.View.next_id_number = 1;

Suave.util.mixInto(HTMLSail.View.prototype,
Sail.View.prototype, {
   //YouCallThisFunction (or write.inside, write.before, write.after, write.ceiling, write.floor
   renderHTML : function(htmlwriter, doc)
   {
      //Log.msg(this)
      htmlwriter(this.generateHTML())
      this.afterWrite();
   },
   afterWrite : function(doc)
   {
      this.locateDomNodes(doc || window.document)
      this.onWritten();      
   },
   
   //YouCallThisFunction
   renderInside : function(elem)
   {
      this.renderHTML(function(html) { elem.innerHTML = html; }, elem.ownerDocument) 
   },
   
   //template help
   defField : function(name, options)
   {
      this.dom_ids[name] = options.id || this.genIdFor(name)
      // if a root is not specified, it's good to specify
      // the first and last nodes so that insertion can work ok
      if (options.first)
	 this.getFirstNode = function() { return this.dom[name]; };
      if (options.last)
	 this.getLastNode = function() { return this.dom[name]; };
      this.dom_opts[name] = options
      return this.dom_ids[name]
   },
   // *** things that you don't use directly but aren't too tricky
   // half-'protected members'
   //dynamically generates an id for an element of the given name
   genIdFor : function(name, options)
   {
      var num = HTMLSail.View.next_id_number++
      if (!name)
         return "anon" + num
      else
         return name + num
   },
   
   //finds all the named elements after they have been inserted
   locateDomNodes : function(doc)
   {
      for (key in this.dom_ids)
         this.dom[key] = doc.getElementById(this.dom_ids[key])
   },
   onWritten : function(){ this.paint() },
   getFirstNode : function() { return this.dom.root; },
   getLastNode : function() { return this.dom.root; }
})

/*
*/