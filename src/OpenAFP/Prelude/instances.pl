use FindBin;
use strict;
chdir $FindBin::Bin;

open LOOKUP, '>Lookups.hs';
select(LOOKUP);

print << '.';

module OpenAFP.Prelude.Lookups where
import OpenAFP.Types
import OpenAFP.Records
import OpenAFP.Internals
import OpenAFP.Prelude.Instances

descLookup :: ChunkType -> String
descLookup = lookupWithDefaultFM descMap "(Unknown)" . show

descMap :: FiniteMap String String
descMap = listToFM [
.

open OUT, '>Instances.hs';
select(OUT);

print << '.';
{-# OPTIONS -fglasgow-exts #-}

module OpenAFP.Prelude.Instances where
import OpenAFP.Types
import OpenAFP.Records
import OpenAFP.Internals

instance Rec T_ where
    recSizeOf c = 1 + (snd $ chunkToPStrLen c)

instance ChunkBuf AFP_ N3 Buffer2 where
    chunkCon = AFP_
    chunkDecon (AFP_ x) = x
    chunkTypeLookup = lookupAFP
    chunkApply = applyAFP

recOf :: (ChunkBuf c n b, Binary (Record r)) => c -> IO r
recOf c = return . fromRecord =<< chunkToRecord c

apply :: (MonadIO m) => (t -> IO a) -> t -> (a -> m b) -> m b
apply ctr c f = f =<< (liftIO $ ctr c)
.

my %module;

open my $fh, "$FindBin::Bin/../Records.hs" or die $!;
my %type;
while (<$fh>) {
    if (/(chunkTypeOf _\w+)\s*-- (.+)/) {
        print LOOKUP "    (show \$ $1, \"$2\"),\n";
    }
    /(0x\S+) -> chunkTypeOf _(\w+)\s*/ or next;
    push @{$type{$2}}, $1;
}

print LOOKUP << '.';
    ("", "(Unknown)")
    ]
.

print << '.';

applyAFP :: (MonadIO m) => N3 -> AFP_ -> (forall a. (Rec a) => (a -> m b)) -> m b
applyAFP x = case x of
.

# now populate things with the headers
my %vals = map { my $k = $_; map +($_ => $k), @{$type{$k}} } grep { !/_/ } keys %type;
foreach my $val (sort keys %vals) {
    print << ".";
    $val -> apply (recOf :: AFP_ -> IO $vals{$val})
.
}
print << ".";
    _        -> apply (recOf :: AFP_ -> IO Unknown)

.

my ($foo, $bar);
foreach my $file (sort(glob("$FindBin::Bin/../Records/*.hs"), glob("$FindBin::Bin/../Records/*/*.hs")), "$FindBin::Bin/../Types.hs") {
    open my $fh, $file or die $!;
    local $/ = '';
    while (<$fh>) {
        if (/module (OpenAFP.Records.\w+)\.(\w+)/) {
            ($foo, $bar) = ($1, $2);
            # $module{$foo}{$bar} ||= "";
        }
        if (/newtype ((\w+)_) = \1 \((\w+), Buffer(\d)/) {
            my ($t, $s, $n, $b) = ($1, $2, $3, $4);
            print << ".";
instance ChunkBuf $t $n Buffer$b where
    chunkCon = $t
    chunkDecon ($t x) = x
    chunkTypeLookup = lookup$s
    chunkApply = apply$s

.
            print << ".";
apply$s :: forall m x. (MonadIO m)
    => $n -> $t -> (forall a. (Rec a) => (a -> m x)) -> m x
apply$s x rec f = case x of
.

            # now populate things with the headers
            my %vals = map { my $k = $_; map +($_ => $k), @{$type{$k}} } grep { /${s}_/ } keys %type;
            foreach my $val (sort keys %vals) {
                print << ".";
    $val -> apply recOf rec (f :: $vals{$val} -> m x)
.
            }
            if ($s =~ /MCF/) {
                print << ".";
    _    -> apply recOf rec (f :: MCF_T -> m x)

.
            }
            else {
                print << ".";
    _    -> apply recOf rec (f :: Unknown -> m x)

.
            }

            print << "." unless $s eq 'T';
instance RecChunk $s $t $n Buffer$b where
    readChunks r = \L$t\EChunks r
    writeChunks r io = do
        cs <- io
        return \$ r { \L$t\EChunks = cs }

instance Rec $t where
    recSizeOf c = $b + (snd \$ chunkToPStrLen c)

.
        }
        if (/data (\w+).*\[(T_)\]/s) {
            my ($s, $t) = ($1, $2);
            print << ".";
instance RecChunk $s $t N1 Buffer1 where
    readChunks r = \L$s\E_Chunks r
    writeChunks r io = do
        cs <- io
        return \$ r { \L$s\E_Chunks = cs }

.
            }
        /^data (\w+) = .*\n([\d\D]+)\}/m or next;
        my $type = $1;
        my $labels = $2;
        $labels =~ s/,//g;
        my @types;
        push @types, $1 while $labels =~ s/::\s*(.+)//;
        my $args = join ' ', map { /Record |\[/ ? '[]' : /NStr/ ? '_NStr' : 0 } @types;
        $args =~ s{^0}{$type{$type}[0]}g if $type{$type};
        $module{$foo}{$type} ||= $args;
        my @labels = ($labels =~ /(\w+)/g);
        my %typeOfLabels;
        @typeOfLabels{@labels} = @types;
        my ($get, $put, $size, $view);
        my $var = 'a00';
        my $vars = '';
        foreach my $label (@labels) {
            $var++;
            my $short = $label;
            $short =~ s/^([^\W_]+)//;
            $short =~ s/^_([^\W_])/$1/;
            if ($short =~ /^\w+_/) {
                $short =~ s/^([^\W_]+)//;
                $short =~ s/^_([^\W_])/$1/;
            }
            my $t = $type;
            $t =~ s/.*_//;
            if ($short eq lc($t)) {
                $short = '';
            }
            $get .= "$var <- get bh; ";
            $put .= "put bh \$ $label r; ";
            $size .= "sizeOf \$ $label r, ";

            my $type = $typeOfLabels{$label};
            if ($type =~ /^!?[IN]\d+/) {
                $view .= "viewField \"$short\" (viewNumber \$ $label r), ";
            }
            elsif ($type =~ /^!?A\d+/) {
                $view .= "viewField \"$short\" (viewString \$ $label r), ";
            }
            elsif ($type =~ /^!?NStr/) {
                $view .= "viewField \"$short\" (viewNStr \$ $label r), ";
            }
            elsif ($type =~ /\[\w+_\]/) {
                $view .= "viewField \"$short\" (viewChunks \$ $label r), ";
            }
            elsif ($type =~ /\[Record \w+\]/) {
                $view .= "viewField \"$short\" (viewData \$ $label r), ";
            }
            else {
                die "Bizzare type: $type"
            }
            $vars .= " $var";
        }
        chop $size; chop $size;
        chop $view; chop $view;
        my $rec = 'rec';
        my $recType = "= fromEnum . $labels[0]";
        $recType = 'r = 0' unless $recType =~ /Type/;
        print << ".";
instance Rec $type where
    recGet bh = do ${get}return \$ $type$vars
    recPut bh r = do ${put}return ()
    recSizeOf r = sum [ $size ]
    recView r = viewRecord (typeOf r) [ $view ]
    recType $recType

.
# recApply f r = [ f ("Type", fno_Type r), f ("_", fno_ r), f ("Reserved", fno_Reserved r), f ("CharacterRotation", fno_CharacterRotation r), f ("MaxBaseOffset", fno_MaxBaseOffset r), f ("MaxCharacterIncrement", fno_MaxCharacterIncrement r), f ("SpaceCharacterIncrement", fno_SpaceCharacterIncrement r), f ("", fno r) ]
        if ($type =~ /_Data/) {
            my $parent = $type;
            $parent =~ s/_Data//;
            print << ".";
instance RecData $parent $type where
    readData r = \L$parent\E_Data r
    writeData r cs = r { \L$parent\E_Data = cs }

.
        }
    }
}

while (my ($key, $val) = each %module) {
    my $name = $key;
    $name =~ s/.*\.//;
    open FH, ">$FindBin::Bin/../Records/$name.hs" or die $!;
    my @sub = sort keys %$val;
    $foo = sub {
        my $t = shift;
        $t =~ s/^\w+_//;
        return $t;
    };
    print FH "module $key (\n";
    foreach my $sub (@sub) {
        if ($sub =~ /_Data/) {
            print FH "    _$sub,\n";
        }
        else {
            print FH "    module $key." . $foo->($sub). ", _$sub,\n";
        }
    }
    print FH ") where\n";
    print FH "import OpenAFP.Types\n";
    print FH "import $key." . $foo->($_) . "\n" for grep !/_Data/, @sub;
    print FH "\n";
    foreach my $sub (@sub) {
        my $pad = ' ' x (4 - length($sub));
        print FH "_$sub$pad :: $sub\n";
        print FH "_$sub$pad = $sub$pad $val->{$sub}\n";
    }
    print FH "\n";
    close FH;
}
