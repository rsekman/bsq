# bsq -- jq for BeautifulSoup

![](https://github.com/rsekman/bsq/raw/refs/heads/master/banner.png)

`bsq` (pronounced "bisque") is a [jq](https://jqlang.org/)-like HTML processor.
It aims to provide the power of [BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/#) with the ease of writing filters with `jq`.

## Examples

Let's use the same example document as BeautifulSoup:
```html
<html>
    <head>
        <title>
            The Dormouse's story
        </title>
    </head>
    <body>
        <p class="title">
        <b>
            The Dormouse's story
        </b>
        </p>
        <p class="story">
        Once upon a time there were three little sisters; and their names were
        <a class="sister" href="http://example.com/elsie" id="link1">
            Elsie
        </a>
        ,
        <a class="sister" href="http://example.com/lacie" id="link2">
            Lacie
        </a>
        and
        <a class="sister" href="http://example.com/tillie" id="link3">
            Tillie
        </a>
        ; and they lived at the bottom of a well.
        </p>
        <p class="story">
        ...
        </p>
    </body>
</html>
```

Some things you can do with bsq are
- Find elements with CSS selectors
```html
% bsq 'find_all("a.sister")' < input.html
<a class="sister" href="http://example.com/elsie" id="link1">
  Elsie
</a>
<a class="sister" href="http://example.com/lacie" id="link2">
  Lacie
</a>
<a class="sister" href="http://example.com/tillie" id="link3">
  Tillie
</a>
```
- Extract contents
```html
% bsq 'find_all("a.sister") | map(stripped_strings)' < input.html
Elsie
Lacie
Tillie
```
- Navigate the tree
```html
% bsq 'find("a.sister") | next_element' < input.html
<a class="sister" href="http://example.com/lacie" id="link2">
  Lacie
</a>
```
```html
% bsq 'find("a#link3") | previous_element' < input.html
<a class="sister" href="http://example.com/lacie" id="link2">
  Lacie
</a>

```
- Access and manipulate attributes
```html
% bsq 'find("a.sister") | .href` < input.html
http://example.com/elsie
```
```html
% bsq 'find("a.sister") | .href = "http://github.com/elsie"` < input.html
<a class="sister" href="http://github.com/elsie" id="link1">
  Elsie
</a>
```
```
% bsq 'find_all("a.sister") | map(.href)' < input.html
http://example.com/elsie
http://example.com/lacie
http://example.com/tillie
```
- Insert and delete elements
  [TODO]

  ## Alternatives

    There are many tools that, like bsq, claim to be "jq but for HTML", but I find they all fail to live up to that promise in various ways.
  - [htmlq](https://github.com/mgdm/htmlq) only provides searching rather than the powerful filtering possible with bsq.
    If jq is grep, sed, and awk for JSON, bsq tries to be that for HTML, but htmlq is only grep.
  - [pup](https://github.com/ericchiang/pup) is another search-only tool.
  - [hq](https://github.com/orf/html-query) converts the HTML into JSON before processing it.
    bsq handles HTML elements as first-class values, but can also output values that can be serialised as JSON.
  - [faq](https://github.com/jzelinskie/faq) is another adaptor that first converts into JSON.
  - [yq](https://github.com/kislyuk/yq) contains xq, which converts XML into JSON. Most HTML is not valid XML.
  - [hq](https://github.com/rbwinslow/hq) uses difficult-to-understand XPath syntax instead of the easy-flowing functional language of jq. 
