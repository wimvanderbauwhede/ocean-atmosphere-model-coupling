my $target_dir = '../RefactoredSources';
my $f77sources = [
'./io/iavrg.f','./io/icord.f','./io/idiag.f','./io/ifopn.f','./io/igtio.f','./io/igzio.f','./io/ihist.f','./io/ihsub.f','./io/irdat.f','./io/irwgd.f','./io/is2po.f','./io/istrt.f',
'./sysdep/ylinux.f', './util/ucaln.f','./util/usubs.f',
];

mkdir "$target_dir/io";
for my $f77src (@{$f77sources}) {
    my $subdir=$f77src;
    $subdir=~s/\w\+.f$//;
    system("cp $f77src $target_dir/$subdir");
}
