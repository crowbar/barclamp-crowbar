/**
 * Copyright 2011-2013, Dell
 * Copyright 2013-2015, SUSE Linux GmbH
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Author: SUSE Linux GmbH
 */

;(function($) {
  function HideShowText(el, options) {
    this.$el = $(el);
     
    this.defaults = {
      hidden: false,
      secret: '',
      target: 'hidetext-secret',
      classes: this.$el.data('hidetext'),
      firstToggle: true
    };
     
    this.options = $.extend(
      this.defaults,
      options
    );

    this.init();
  }
   
  HideShowText.prototype.init = function() {
    var self = this;
    
    this.$el.data(this.options.target, this.$el.text());
    self.createSecretString();
    self.toggleText();
     
    this.$el.on('click', 'div.toggle-text', function(e) {
      e.preventDefault();
      self.toggleText();
    });
  };

  HideShowText.prototype.createSecretString = function() {
    for (var i = 0; i < this.$el.text().length; i++)  {
      this.options.secret += "&#149;";
    }
  }
   
  HideShowText.prototype.toggleText = function() {
    if (this.options.firstToggle) {
      this.$el.text("");
      this.$el.append("<span class=\"hidetext-text\">" + this.options.secret + "</span> <div class=\"toggle-text toggle-text-show " + this.options.classes + "\">&nbsp;</div>");
      this.options.firstToggle = false;
      this.options.hidden = true;
    } else {
      var text;

      if (this.options.hidden) {
        text = this.$el.data(this.options.target);
        this.options.hidden = false;
      } else {
        text = this.options.secret;
        this.options.hidden = true;
      }

      this.$el
        .children('.hidetext-text')
          .html(text)
        .end()
        .children('.toggle-text')
          .toggleClass('toggle-text-show toggle-text-hide');
    }
  };
   
  $.fn.hideShowText = function(options) {
    return this.each(function() {
      new HideShowText(this, options);
    });
  };
}) (jQuery);
