We can generate sets using the syntax
 p([a,b,c],[1,2])
and it will produce the set:
 [p(a,1),p(a,2),p(b,1),...,p(c,2)]

Here is an example:
  $ touist --solve --sat - <<'EOF'
  > bigand $i in p([a,b,c,d],[1,2,3],ahah,[0]): $i end
  > EOF
  1 p(d,3,ahah,0)
  1 p(a,3,ahah,0)
  1 p(c,3,ahah,0)
  1 p(d,2,ahah,0)
  1 p(b,1,ahah,0)
  1 p(d,1,ahah,0)
  1 p(a,1,ahah,0)
  1 p(c,1,ahah,0)
  1 p(b,3,ahah,0)
  1 p(c,2,ahah,0)
  1 p(a,2,ahah,0)
  1 p(b,2,ahah,0)
