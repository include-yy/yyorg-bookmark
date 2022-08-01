// ==UserScript==
// @name        yyob-add-bookmark
// @namespace   http://tampermonkey.net/
// @version     0.1
// @description use org-protocol and tm-script to add bookmark
// @author      include-yy
// @match       *://*/*
// @grant       unsafeWindow
// @grant       GM_registerMenuCommand
// ==/UserScript==

(function() {
    'use strict'; // Your code here...

    // all templates
    // [key, description, accesskey]
    // comment or uncomment to add/remove item
    let all = [
        ['yyobp', 'Add Bookmark', 'a'],
	['L', 'add bk 2', 'p']
    ];

    let i = 0;
    // https://stackoverflow.com/questions/25750183/how-to-create-a-toolbar-button-for-a-chrome-tampermonkey-user-script
    // how to add MenuCommand
    for (i = 0; i < all.length; i++)
    {
        let name = all[i][0];
        let desc = all[i][1];
        let hotkey = all[i][2];
        GM_registerMenuCommand(desc, function() {
            main(name);
        }, hotkey);
    }
    // https://github.com/toure00/org-capture-tag-bookmark
    // how to capture link and description
    function main (key) {
        location.href = 'org-protocol://capture?template=' + key +
            '&url=' + encodeURIComponent(location.href) +
            '&title=' + encodeURIComponent(document.title) +
	    '&body=' + encodeURIComponent(window.getSelection());
    }

    // my original thought was to use radio/checkbox dialog to add or remove template to use
    // but I found it easier to just add/remove a list in a list variable :p
    // if you want to do like this, you can refer to
    // https://stackoverflow.com/questions/11668111/how-do-i-pop-up-a-custom-form-dialog-in-a-greasemonkey-script
    // and https://github.com/toure00/org-capture-tag-bookmark
    // if you want to use jQuery, just paste blow line to the ==userscript== block
    // @require     https://code.jquery.com/jquery-2.1.4.min.js
})();
