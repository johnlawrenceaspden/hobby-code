
\version "2.18.2"
% automatically converted by musicxml2ly from /home/john/hobby-code/lilypond/MOOC.xml

\header {
  encodingsoftware = "MuseScore 2.0.3"
  encodingdate = "2018-05-15"
}

\layout {
  \context {
    \Score
    skipBars = ##t
  }
}
PartPOneVoiceOne =  \relative gis' {
  \repeat volta 2 {
    \clef "treble" \key e \major \time 3/4 gis4 e4. fis8 | % 2
    <fis, dis'>4 <fis a b>2 | % 3
    <gis b gis'>4 e'4. fis8 | % 4
    fis4 ais4 b4 | % 5
    \acciaccatura { b,16 e16 gis16 } b4 e,4. gis16 fis16 | % 6
    cis4 b4 \acciaccatura { a16 b16 a32 gis32 } a4 | % 7
    gis4 e'4. cis8 | % 8
    \clef "bass" ais2 b4
  }
  | % 9
  \clef "treble" R2.*24 \bar "|."
}

PartPOneVoiceTwo =  \relative b {
  \repeat volta 2 {
    \clef "treble" \key e \major \time 3/4 b4 b4 cis4 s2. | % 3
    r4 b4 cis4 | % 4
    dis4 <cis e>4 <dis fis>4 | % 5
    r4 b4 cis4 s2. s2. | % 8
    \clef "bass" gis4 e4 dis4
  }
  | % 9
  \clef "treble" s4*33 s4*39 \bar "|."
}

PartPOneVoiceFive =  \relative e, {
  \repeat volta 2 {
    \clef "bass" \key e \major \time 3/4 e4 gis4 a4 | % 2
    b4 cis4 dis4 | % 3
    e4 gis4 ais4 | % 4
    b4 fis4 b,8 a8 | % 5
    gis4 gis'4 a,4 | % 6
    R2. | % 7
    r4 e'2 e,4 g4 b4
  }
  R2.*24 \bar "|."
}

PartPOneVoiceSix =  \relative gis, {
  \repeat volta 2 {
    \clef "bass" \key e \major \time 3/4 s1*3 | % 5
    gis2 r4 | % 6
    <b fis'>4 <cis e>4 <dis fis>4 | % 7
    e4 <cis, cis'>4 <c c'>4 c2 b4
  }
  R2. s2*15 s4*39 \bar "|."
}


% The score definition
\score {
  <<
    \new PianoStaff <<
      \set PianoStaff.instrumentName = "Piano"
      \set PianoStaff.shortInstrumentName = "Pno."
      \context Staff = "1" <<
        \context Voice = "PartPOneVoiceOne" { \voiceOne \PartPOneVoiceOne }
        \context Voice = "PartPOneVoiceTwo" { \voiceTwo \PartPOneVoiceTwo }
      >> \context Staff = "2" <<
        \context Voice = "PartPOneVoiceFive" { \voiceOne \PartPOneVoiceFive }
        \context Voice = "PartPOneVoiceSix" { \voiceTwo \PartPOneVoiceSix }
      >>
    >>

  >>
  \layout {}
  \midi {}
}

