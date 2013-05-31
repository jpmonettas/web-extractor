Web Extractor
=============

This is a small DSL written completely in Common Lisp to make a little easier the task of download and structure information from the web.

It gives you a language to create a hierarchy of objects with instructions on how to find the info to fill their fields in a given subcontext while
hidding things like fixing wrongly written web pages and stuff like that.

As an example, consider a website like http://twitaholic.com/

It serves web pages like :

```html
<html>
...
 <body>
 ...
 <table ...>
  <tbody>
   <tr>...</tr>
   <tr>
     <td class="statcol_num">1</td>
     <td class="statcol_img">...</td>	
     <td class="statcol_name">
	<a href="/justinbieber/" title="Justin Bieber (aka justinbieber)">Justin Bieber (justinbieber)</a>
     </td>
     ...
   </tr>
   <tr class="alt">
	<td class="statcol_num">2</td>
	<td class="statcol_img">...</td>
	<td class="statcol_name">
		<a href="/ladygaga/" title="Lady Gaga (aka ladygaga)">Lady Gaga (ladygaga)</a>
	</td>
	...
   </tr>
  </tbody>
</table>
</body>
</html>
```

and clicking on each name link will gives you a detail page like :

```html
<html>
  ...
  <body>
  ...

<div id="user_main">
  <div id="user_info">
   ...
    <a href="http://twitter.com/justinbieber" rel="nofollow">Twitter Page</a> | <a href="http://www.youtube.com/justinbieber" rel="nofollow">Website</a>
   ...
    <div id="user_details">
      <ul>
        <li>Joined Twitter on 2009-03-28 11:41:22</li>
	...
      </ul>
       ...
 </div>
 </div>
```

Now how to use this language to download all those twitter users details 

For the detail pages you can write something like :

```lisp
(def-web-extractor twitaholic-details-map
    ((join-date :finder (compose 
			 (xpath-finder "//div[@id='user_details']/ul/li[1]") 
			 (regexp-finder "(\\d{4}-\\d{2}-\\d{2})")))
     (website :finder (xpath-finder "//div[@id='user_info']/a[2]/@href"))))
```

For the root page you can parse the first 200 in the collection as :

```lisp
(def-web-extractor twitaholic-map
    ((number :finder (xpath-finder "/tr/td[1]"))
     (name :finder (xpath-finder "/tr/td[3]/a"))
     (followers :finder (xpath-finder "/tr/td[6]"))
     (details :follow twitaholic-details-map :finder (xpath-finder "/tr/td[3]/a/@href"))))
     
(def-web-extractor twitaholics-map
    ((twitaholics :collection twitaholic-map
		  :splitter (xpath-splitter "/html/body/div/div[4]/div[2]/table/tbody/tr[position()>1]")
		  :limit 200)))

```

and then extract all that info in a s-exp or a JSON like format with :

```lisp
(extract :url "http://twitaholic.com/" :struct-map twitaholics-map)
```

For more examples look in the tests directory.

The dsl is very flexible because you can write any common lisp function as a :finder, :splitter or whatever.

It already support some kind of type casting also. 