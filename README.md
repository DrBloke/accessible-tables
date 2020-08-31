# Complex Tables
Creating HTML tables can be hard, especially if they have multi-level column or row headings; get it slightly wrong and screen readers will get confused and you inadvertently make your page inaccessible. 

This library takes away your pain. Get your data into a simple format and initialise your table. Then tell it what you want your heading structure to be. And hopefully that's it; you'll be given an HTML node to put into your page. If not, for example if one of your rows has the wrong number of data points, then you should be told why.

In addition, to make your tables accessible to screen reader users, you should probably put in extra mark-up that adds elements to your table that might not be necessary for the desired visual goad. This libary helps you to do that without any thought.

## Examples
1. A table with column and row headings
2. A table with column headings only that is accessible to screen reader users
3. A table with neither column nor row headings that is accessible to screen reader users
4. A table with multi-level column headings and single-level row headings
coming soon...
5. A table with multi-level row headings and single-level column headings
6. A table with multi-level row headings and multi-level column headings
7. A table with column groups
8. A table with row groups

## Inspiration
This project was inspired and guided by the following:
* [W3.org Web Accessibility Tutorials](https://www.w3.org/WAI/tutorials/tables/multi-level/)
* [MDN Web Docs pages on HTML tables](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)
* [CSS Tricks Complete Guide to the Table Element](https://css-tricks.com/complete-guide-table-element/)
* One of the motivations for this project as to make a tabular view for screen reader users in Decio Batagglia's [elm-char-builder library] (https://package.elm-lang.org/packages/data-viz-lab/elm-chart-builder/latest/)

