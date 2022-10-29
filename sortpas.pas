{$A+,B-,D+,E+,F-,G-,I-,L+,N+,O-,P-,Q-,R-,S-,T-,V+,X+,Y+}
{$M 65500,0,655360}
uses dos,crt,zm01unit;
const n=30000;
      NumOfLoops=10;

const limmm:integer=1*n;

type maintype=word;
     vector=array[1..n] of maintype;
var t1,t2:Time;
    a:vector;
    i:integer;

const      t:integer=2;
      {t=0: qsort}
      {t=1: zmradix shl shr}
      {t=2: zmradix hi lo}
      {t=3: radix sjuvord}

procedure Insert;
  var i,j:integer;
      buf:maintype;
  begin
    for i:=2 to N do
      begin
        buf:=a[i];j:=i-1;
        while (j>=1)and(buf<a[j])do
          begin
            a[j+1]:=a[j];
            dec(j);
          end;
        a[j+1]:=buf;
      end;
  end;

procedure RandomArray;
  var i:integer;
  begin
    for i:=1 to N do a[i]:=random(limmm);
  end;

procedure quicksort(m:integer);

  procedure Bubble(l,r:integer);
    var i,j:integer;
        buf:maintype;
    begin
      for i:=r downto l+1 do
        for j:=l to i-1 do
          begin
            if a[j]>a[j+1]then
              begin
                buf:=a[j];a[j]:=a[j+1];a[j+1]:=buf;
              end;
          end;
    end;

  procedure Sort(l,r:integer);
    var i,j:integer;
        y,z:maintype;
    begin
      i:=l;j:=r;
      y:=a[i div 2 + j div 2];
      repeat
        while a[i]<y do i:=i+1;
        while a[j]>y do j:=j-1;
        if i<=j then begin
          z:=a[i];a[i]:=a[j];
          a[j]:=z;i:=i+1;j:=j-1;
        end;
      until i>j;
      if l<j then if j-l+1>m then Sort(l,j) else Bubble(l,j);
      if i<r then if r-i+1>m then Sort(i,r) else Bubble(i,r);
    end;

  begin
    Sort(1,N);
  end;

function ArrayIsInOrder:boolean;
  var i:integer;
  begin
    for i:=1 to N-1 do
      if a[i]>a[i+1]then begin ArrayIsInOrder:=false;exit;end;
    ArrayIsInOrder:=true;
  end;

{-------------------------------------------------------------------}

procedure zmradix1;
  const Mp=256;
  var Count,Count2,Count3:array[-1..Mp-1]of integer;
      k:integer;

  procedure zm(l,r:integer;pass:word);
    var bbuf,q,p,i:integer;
        st,buf1,buf2:word;
    begin
      if l+1>=r then exit;
      st:=8-pass*8;
      for i:=0 to Mp-1 do Count[i]:=0;
      for i:=l+1 to r do inc(Count[a[i]shl(st)shr 8]);
      Count[0]:=Count[0]+l;
      for i:=1 to Mp-1 do inc(Count[i],Count[i-1]);
      Count2:=Count;Count2[-1]:=-1;q:=Mp-1;
      repeat
        i:=Count[q];buf1:=a[i];
        repeat
          bbuf:=buf1 shl(st)shr 8;p:=Count[bbuf];
          buf2:=a[p];a[p]:=buf1;
          dec(Count[bbuf]);buf1:=buf2;
        until p=i;
        while Count2[q-1]=Count[q]do dec(q);
      until q<1;
  end;

  begin
    zm(0,n,1);

    Count3:=Count2;Count3[-1]:=0;
    for k:=-1 to Mp-2 do zm(Count3[k],Count3[k+1],0);
  end;


procedure zmradix3;
  const m=8;
  const Mp=256;
  var Count,Count2,Count3:array[-1..Mp-1]of integer;
      k:integer;

  procedure Bubble(l,r:integer);
    var i,j:integer;
        buf:maintype;
    begin
      for i:=r downto l+1 do
        for j:=l to i-1 do
          begin
            if a[j]>a[j+1]then
              begin
                buf:=a[j];a[j]:=a[j+1];a[j+1]:=buf;

              end;
          end;
    end;

  procedure Sort(l,r:integer);
    var i,j:integer;
        y,z:maintype;
    begin
      i:=l;j:=r;
      y:=a[i div 2 + j div 2];
      repeat
        while a[i]<y do i:=i+1;
        while a[j]>y do j:=j-1;
        if i<=j then begin
          z:=a[i];a[i]:=a[j];
          a[j]:=z;i:=i+1;j:=j-1;
        end;
      until i>j;
      if l<j then if j-l+1>m then Sort(l,j) else Bubble(l,j);
      if i<r then if r-i+1>m then Sort(i,r) else Bubble(i,r);
    end;


  procedure zm(l,r:integer;pass:word);
    var bbuf,q,p,i:integer;
        buf1,buf2:word;
    begin
      if l+1>=r then exit;
      for i:=0 to Mp-1 do Count[i]:=0;

      if pass=1 then
        for i:=l+1 to r do inc(Count[hi(a[i])])
      else
        for i:=l+1 to r do inc(Count[lo(a[i])]);

      Count[0]:=Count[0]+l;
      for i:=1 to Mp-1 do inc(Count[i],Count[i-1]);
      Count2:=Count;Count2[-1]:=-1;q:=Mp-1;

    if(pass=1)then
      repeat
        i:=Count[q];buf1:=a[i];
        repeat
          bbuf:=hi(buf1);p:=Count[bbuf];
          buf2:=a[p];a[p]:=buf1;
          dec(Count[bbuf]);buf1:=buf2;
        until p=i;
        while Count2[q-1]=Count[q]do dec(q);
      until q<1
    else
      repeat
        i:=Count[q];buf1:=a[i];
        repeat
          bbuf:=lo(buf1);p:=Count[bbuf];
          buf2:=a[p];a[p]:=buf1;
          dec(Count[bbuf]);buf1:=buf2;
        until p=i;
        while Count2[q-1]=Count[q]do dec(q);
      until q<1;
  end;

var iii,xxx:integer;

  begin
    zm(0,n,1);
    Count3:=Count2;Count3[-1]:=0;

    for k:=-1 to Mp-2 do
      begin
        xxx:=Count3[k+1]-Count3[k];
        if(xxx>=100)then
          begin
            zm(Count3[k],Count3[k+1],0)
          end
        else

          if(xxx>m)then Sort(Count3[k]+1,Count3[k+1])
          else Bubble(Count3[k]+1,Count3[k+1]);
      end;
  end;

procedure radix;
  const m=8;Mp=1 shl m;W=16;
  var y:vector;
      Count:array[0..Mp-1]of integer;
      i,pass,k:integer;t:boolean;
      bbuf,pass_m,mm,r:word;
  begin
    t:=true;mm:=m;r:=W;k:=1;
    if W mod m =0 then k:=0;
    for pass:=0 to W div m+k-1 do
      begin
        if r<m then mm:=r else dec(r,m);
        pass_m:=pass*m;
        for i:=0 to Mp-1 do Count[i]:=0;
        for i:=1 to N do inc(Count[(a[i]shl(16-pass_m-mm))shr(16-mm)]);
        for i:=1 to Mp-1 do Count[i]:=Count[i]+Count[i-1];
        if t then
          for i:=N downto 1 do
            begin
              bbuf:=(a[i]shl(16-pass_m-mm))shr(16-mm);
              y[Count[bbuf]]:=a[i];dec(Count[bbuf]);
            end
        else
          for i:=N downto 1 do
            begin
              bbuf:=(y[i]shl(16-pass_m-mm))shr(16-mm);
              a[Count[bbuf]]:=y[i];dec(Count[bbuf]);
            end;
        t:=not t;
      end;
    if not t then begin a:=y;end;
  end;

procedure process;
  var s:string;
  begin
    case t of
      0:s:='qsort';
      1:s:='zmradix shl shr';
      2:s:='zmradix hi lo';
      3:s:='radix sjuvord';
    end;
    centerwrite(s,3);
    centerwrite('Время начала работы процедуры:',7);
    centerwrite(timetostr(t1),9);
    centerwrite('Время конца работы процедуры:',11);
    centerwrite(timetostr(t2),13);timing(t1,t2);
    centerwrite('Время, затраченное на сортировку '+IntToStr(NumOfLoops)+' случайных массивов из '
    +IntToStr(n)+' чисел:',15);
    centerwrite(timetostr(t1),17);readkey;
  end;

var k:integer;
    xx,kk:extended;

begin
  highvideo;clrscr;write('v.2.01');randomize;
  writeln('Введите номер сортировки:');
  writeln('0: qsort');
  writeln('2: zmradix hi lo');
  writeln('3: radix sjuvord');

  readln(t);

  writeln('Введите k  (k = L/N) :');

  readln(k);

  xx:=N;
  kk:=k;
  xx:=xx*kk;
  if(xx>65535)then
    begin
      writeln('Слишком высокое значение k');
      exit;
    end;

  limmm:=k*n;

  with t1 do gettime(h,m,s,hd);
    for i:=1 to NumOfLoops do
      begin
        RandomArray;
        case t of
          0:quicksort(8);
          1:zmradix1;
          2:zmradix3;
          3:radix;
        end;
      end;
  with t2 do gettime(h,m,s,hd);
  if not ArrayIsInOrder then begin write('Error!');readkey;end;
  process;
end.
