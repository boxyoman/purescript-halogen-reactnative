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


exports.element_ = function(el, attrs) {
  return React.createElement.apply(
    null,
    [el, attrs].concat((attrs && attrs.children) || [])
  );
};


exports.handlerProp = function(key, f) {
  var prop = {};
  prop[key] = function (e) {
    console.log(key, e);
    f(e)();
  };
  return prop;
};


exports.registerComponent = function(name){
  return function(element){
    return function(){
      var componentRef;
      const updateState = function(element){
        return function () {
          console.log(componentRef);
          if(componentRef === undefined) {
            return ;
          }
          componentRef.setState({element: element});
        }
      }
      const component = createReactClass({
        getInitialState: function() {
          componentRef = this;
          return {element: element};
        },
        render: function () {
          return this.state.element;
        }
      });
      RN.AppRegistry.registerComponent(name, function(){ return component; });
      return updateState;
    }
  }
};
