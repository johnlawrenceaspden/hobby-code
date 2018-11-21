% ŵ˘ˆ┌ ┐⁀⁀© — –
% U+0311 = ̑
% U+0361 = ͡
% U + 032E = ̮

\version "2.10.20"
#(ly:set-option 'point-and-click #f)

\paper
{
	indent = 0.0
	line-width = 175 \mm
	between-system-space = 23.0 \mm
	%between-system-padding = #1
	%ragged-bottom = ##t
	%top-margin = 0.1 \mm
	%ragged-last-bottom
	%bottom-margin = 0.1 \mm
	%foot-separation = 0.1 \mm
	%head-separation = 0.1 \mm
	%after-title-space = 0.1 \mm
	%before-title-space = 0.1 \mm
	%horizontal-shift
	%system-count
	%left-margin
	%paper-width
	%paper-height
	%print-page-number
	%printfirst-page-number
	%between-title-space = 0.1 \mm
	%printallheaders
	%systemSeparatorMarkup
}

\header
{
	%dedication = ""
	title = "Dear Lord and Father of Mankind"
	%subtitle = ""
	subsubtitle = "(SSATBB)"
	% poet = \markup{ \italic Text: }
	% composer = \markup{ \italic Music: }
	%meter = ""
	%opus = ""
	%arranger = ""
	%instrument = ""
	%piece = \markup{\null \null \null \null \null \null \null \null \null \null \null \null \null \italic Slowly \null \null \null \null \null \note #"4" #1.0 = 70-100}
	%breakbefore
	%copyright = ""
	tagline = ""
}


global =
{
	\override Staff.TimeSignature #'style = #'()
	\time 4/4
	\key es \major
}

sopWords = \lyricmode
{
	\override Score . LyricText #'font-size = #-2
	% \override Score . LyricText #'font-name = #"Gentium"
	% \override Score . LyricText #'self-alignment-X = #-1
	\set stanza = "1. "
	Dear Lord and Fa -- ther of man -- kind,
	For -- give our foo -- lish ways;
	Re -- clothe us in our right -- ful mind,
	In pur -- er lives Thy ser -- vice find,
	In deep -- er rev -- 'rence, praise.
	In deep -- er rev -- 'rence, praise.
}
sopWordsTwo = \lyricmode
{
	\set stanza = "2. "
	In sim -- ple trust like theirs who heard,
	Be -- side the Syr -- ian sea,
	The gra -- cious cal --  ling of the Lord,
	Let us, like them, with -- out a word,
	Rise up and fol -- low Thee.
	Rise up and fol -- low Thee.
}
sopWordsThree = \lyricmode
{
	\set stanza = "3. "
	O Sab -- bath rest by Ga -- li -- lee,
	O calm of hills a -- bove,
	Where Je -- sus knelt to share with Thee
	The si -- lence of e -- ter -- ni -- ty,
	In -- ter -- pre -- ted by love!
	In -- ter -- pre -- ted by love!
}
sopWordsFour = \lyricmode
{
	\set stanza = "4. "
	With that deep hush sub -- du -- ing all
	Our words and works that drown
	The ten -- der whis -- per of Thy call,
	As noise -- less let Thy bless -- ing fall
	As fell Thy man -- na down.
	As fell Thy man -- na down.
}
sopWordsFive = \lyricmode
{
	\set stanza = "5. "
	Drop Thy still dews of qui -- et -- ness,
	Till all our stri -- vings cease;
	Take from our souls the strain and stress,
	And let our or -- dered lives con -- fess
	The beau -- ty of Thy peace.
	The beau -- ty of Thy peace.
}
sopWordsSix = \lyricmode
{
	\set stanza = "6. "
	Breathe through the heats of our de -- sire
	Thy cool -- ness and Thy balm;
	Let sense be dumb, let flesh re -- tire;
	Speak through the earth -- quake, wind, and fire,
	O still, small voice of calm.
	O still, small voice of calm.
}
sopWordsSeven = \lyricmode
{
	\set stanza = "7. "
}
altoWords = \lyricmode
{

}
tenorWords = \lyricmode
{

}
bassWords = \lyricmode
{

}

\score
{
	%\transpose es' e'
	<<
		\new Staff %\with
		%{
		%		\consists "Ambitus_engraver"
		%}
		<<
			%\set Score.midiInstrument = "Orchestral Strings"
			%\set Score.midiInstrument = "Choir Aahs"
			\new Voice = "sopranos" %\with
			%{
				%\consists "Ambitus_engraver"
			%}
			{
				\voiceOne
				\global
				%\override Rest #'direction = #'0
				%\override Score.MetronomeMark #'transparent = ##t
				\override Score.MetronomeMark #'stencil = ##f
				\tempo 4 = 82
				\partial 4
				%es'4 bes'4. bes'8 <c'' aes'> (<bes'>) g' (aes') bes'4. bes'8 es'4 es' aes' g' d' es' bes2. \bar "" \break
				es'4 bes'4. bes'8 <c'' aes'> (<bes' g'>) g' (aes') bes'4. bes'8 es'4 es' aes' g' d' es' bes2. \bar "" \break
				%es'4 bes'4. bes'8 c'' (bes') g' (aes') bes'4. bes'8 es'4 es' aes' g' d' es' bes2. \bar "" \break
				bes4 c'4 <g' es'> f' es' d'4 <aes'> g' f' es'4. es'8 <c'' aes'>4 aes' f' g'8 (aes') bes'4 \bar "" \break
				c''8 (d'') <es'' bes'>4 <bes' g'> c''8 (bes') g' (aes') <bes'>2. \tempo 4 = 76 bes'4 \tempo 4 = 73 <aes'>4 c'4 es'4. \tempo 4 = 71 d'8 \tempo 4 = 73 es'2.
				\bar "|."
			}

			\new Voice = "altos"
			{
				\voiceTwo
				%es'4 es'4. es'8 es'8 (g') es'8 (es') es'4. es'8 bes4 bes4 c' es' d' c' bes2.
				es'4 es'4. es'8 es'8 (<d'>) es'8 (es') d'4. es'8 bes4 bes4 c' es' d' bes bes2.
				%es'4 es'4. es'8 es'4 es'4 es'4. es'8 bes4 bes4 c' es' d' bes bes2.
				bes4 c' c' c' c' d' <f'> d' d' es'4. es'8 <c'>4 c' d' es'8 (f') g'4
				f'8 (f') es'4 es' es'4 es'4 d'2. es'4 c' c' c'4.
				%bes8 bes2.
				d'8 bes2.
				%%d'8 bes2.
				%%%bes8 bes2.
				%d'8 bes2.
				%bes8 bes2.
			}

			\new Lyrics = sopranos { s1 }
			\new Lyrics = sopranosTwo { s1 }
			\new Lyrics = sopranosThree { s1 }
			\new Lyrics = sopranosFour { s1 }
			\new Lyrics = sopranosFive { s1 }
			\new Lyrics = sopranosSix { s1 }
			%\new Lyrics = sopranosSeven { s1 }
			%\new Lyrics = altos { s1 }
			%\new Lyrics = tenors { s1 }
			%\new Lyrics = basses { s1 }
		>>


		\new Staff
		<<
			\clef bass
			\new Voice = "tenors"
			{
				\voiceThree
				\global
				%bes4 bes4. bes8 c'8 (bes) bes8 (c') bes4. bes8 g4 g4 aes bes aes aes g2.
				bes4 bes4. bes8 c'8 (bes) bes8 (c') bes4. bes8 g4 g4 aes bes aes g g2.
				%bes4 bes4. bes8 bes4 bes4 bes4. bes8 g4 g4 aes bes aes g g2.
				g4 aes g aes g aes <c'> bes aes bes4. bes8 <aes>4 aes aes bes8 (d') es'4
				c'8 (bes) bes4 bes bes4 bes4 aes2. bes4 aes aes aes4.
				%aes8 g2.
				aes8 g2.
				%%aes8 g2.
				%aes8 g2.
				%f8 g2.
			}

			\new Voice = "basses"
			{
				\voiceFour
				%g4 g4. g8 aes8 (g) g8 (aes) g4. g8 es4 es4 f g f bes, <es bes,>2.
				g4 g4. g8 aes8 (g) g8 (aes) f4. g8 es4 es4 f g f es <es bes,>2.
				%g4 g4. g8 g4 g4 g4. g8 es4 es4 f g f es <es bes,>2.
				es4 es g f es f <aes> g f <g es>4. <g bes,>8 <aes,>4 es f g8 (d) <bes bes,>4
				aes8 (d) <g g,>4 <g es> g4 g4 f2. g4 <f> <es c> es4.
				<f d>8 <es>2.
				%<f d>8 <es \parenthesize bes,>2.
				
				%<f bes,>8 <es bes,>2.
				%<f bes,>8 <es>2.
				%<d>8 <es>2.
				%<f>8 <es bes,>2.
				%<bes,>8 <es bes,>2.
			}
		>>
		\context Lyrics = sopranos \lyricsto sopranos \sopWords
		\context Lyrics = sopranosTwo \lyricsto sopranos \sopWordsTwo
		\context Lyrics = sopranosThree \lyricsto sopranos \sopWordsThree
		\context Lyrics = sopranosFour \lyricsto sopranos \sopWordsFour
		\context Lyrics = sopranosFive \lyricsto sopranos \sopWordsFive
		\context Lyrics = sopranosSix \lyricsto sopranos \sopWordsSix
		%\context Lyrics = sopranosSeven \lyricsto sopranos \sopWordsSeven
		%\context Lyrics = altos \lyricsto altos \altoWords
		%\context Lyrics = tenors \lyricsto tenors \tenorWords
		%\context Lyrics = basses \lyricsto basses \bassWords
	>>
	
	\midi { }
	\layout
	{	
		\context
		{
			\Lyrics
			\override VerticalAxisGroup #'minimum-Y-extent = #'(0 . 0)
		}
		%\context
		%{
			%\Voice
			%\consists Ambitus_engraver
		%}
	}
}

\markup
{
	\column
	{
		\line{\italic Text: John Greenleaf Whittier (1807 - 1892), in the \italic {Atlantic Monthly,} April 1872}
		\line{\italic Melody: Charles Hubert Hastings Parry (1848 - 1918), 1888}
		\line{\italic Parts: Mark Hamilton Dewey (b. 1980), 2007}
		%\line{\italic Parts: Mark Hamilton Dewey (b. 1980), 8-9 May 2007}
		%\line{\italic {Tune Name:} Repton}
		%\line{\italic {Poetic Meter:} 8 6 8 8 6 6}
	}
}