\version "2.7.40"
\header {
	crossRefNumber = "1"
	footnotes = ""
	subtitle = "https://thesession.org/tunes/60#setting60"
	tagline = "Lily was here 2.18.2 -- automatically converted from ABC"
	title = "Lilting Banshee, The"
}
voicedefault =  {
\set Score.defaultBarType = ""

\repeat volta 2 {
\time 6/8 \key a \dorian   \repeat volta 2 {   e'8    a'8    a'8    e'8    a'8  
  a'8  \bar "|"   b'8    a'8    b'8    g'4    a'8  \bar "|"   b'8    e''8    
e''8    e''8    d''8    b'8  \bar "|"   d''8    e''8    fis''8    g''8    
fis''8    g''8  \bar "|"   e''8    a'8    a'8    e''8    a'8    a'8  \bar "|"   
b'8    a'8    b'8    g'4    a'8  \bar "|"   b'8    e''8    e''8    e''8    d''8 
   b'8  \bar "|"   d''8    b'8    a'8    a'4.  }   e''8    a''8    a''8    a''8 
   g''8    e''8  \bar "|"   d''8    b'8    a'8    g'4    a'8  \bar "|"   b'8    
e''8    e''8    e''8    d''8    b'8  \bar "|"   d''8    e''8    fis''8    g''8  
  fis''8    g''8  \bar "|"   e''8    a''8    a''8    a''8    g''8    e''8  
\bar "|"   d''8    b'8    a'8    g'4    a'8  \bar "|"   b'8    e''8    e''8    
e''8    d''8    b'8  \bar "|"   d''8    b'8    a'8    a'4.  }   
}

\score{
    <<

	\context Staff="default"
	{
	    \voicedefault 
	}

    >>
	\layout {
	}
	\midi {}
}
