'use strict';

// module ReactNative.Basic


var createReactClass = require('create-react-class');
var React = require('react');
var RN = require('react-native');
var R = require('ramda');

// React classes
exports.prop = function (key, value) {
  var prop = {};
  prop[key] = value;
  return prop;
};

exports.emptyProps = {};

exports.concatProps = function(props1, props2) {
  return R.mergeDeepRight(props1, props2);
};


exports.element_ = function(rclass, attrs) {
  return React.createElement.apply(
    null,
    [rclass, attrs].concat((attrs && attrs.children) || [])
  );
};


exports.handlerProp = function(key, f) {
  var prop = {};
  prop[key] = function (e) {
    f(e)();
  };
  return prop;
};

exports.updateState_ = function (element, stuff) {
  return function () {
    if (stuff.self === null) {
    } else {
      stuff.self.setState({element : element});
    }
  };
};

exports.mkComponent = function(element) {
  const stuff = {
    self: null,
  };
  const rclass = createReactClass({
    getInitialState: function () {
      stuff.self = this;
      return { element: element };
    },
    render: function () {
      return this.state.element;
    },
  });
  return {rclass: rclass, self: stuff,};
};

exports.registerComponent = function(name){
  return function(rclass){
    return function(){
      RN.AppRegistry.registerComponent(name, function(){ return rclass; });
    }
  }
};
