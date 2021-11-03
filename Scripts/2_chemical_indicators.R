source("Scripts/1_data_import_arranging.r")

df_nested 

fingerprint_OB(obmolRefs = obmol(df_nested$canonical_sdf[[100]]), "FP4" )

test1 <- fingerprint_OB(obmolRefs = obmol(df_nested$canonical_sdf[[100]]), "FP4" )
length(test1)

obmol(df_nested$canonical_sdf[[1]] )

forEachMol("SMILES",df_nested$canonical_smiles[[1]])

lapply(df_nested$canonical_sdf,MW)
lapply(df_nested$canonical_sdf, bondblock )
lapply(df_nested$canonical_sdf, bonds)
lapply(df_nested$canonical_sdf, rings)

# /////////////////////////////////////////////////


cdk.version()

mols <- parse.smiles(df_nested$canonical_smiles[[1]])[[1]]

get.atoms(mols)
get.bonds(mols)
view.molecule.2d(mols)
get.point3d(get.atoms(mols))

dc <- get.desc.categories()
dc
dn <- get.desc.names(dc[4])
dn
eval.desc(mols, dn[4])
test1<-eval.desc(mols, dn)
test1
t(test1)

convert.implicit.to.explicit(mols)
get.tpsa(mols)
get.xlogp(mols)
unique(unlist(sapply(get.desc.categories(), get.desc.names)))

mols <- parse.smiles(df_nested$canonical_smiles[[1]])

fp <- get.fingerprint(mols[[1]], type='maccs')
fps <- lapply(mols, get.fingerprint, type='extended')
fp.sim <- fingerprint::fp.sim.matrix(fps, method='tanimoto')
fp.dist <- 1 - fp.sim

fp.dist


sp <- get.smiles.parser()
molecule <- parse.smiles(df_nested$canonical_smiles[[1]])[[1]]
convert.implicit.to.explicit(molecule)
formula <- get.mol2formula(molecule,charge=0)
formula
formula@mass
formula@charge
formula@isotopes
formula@string

formula <- set.charge.formula(formula, charge=1)
formula


formula <- get.mol2formula(molecule,charge=0)
isotopes <- get.isotopes.pattern(formula,minAbund=0.1)
isotopes
