from Bio import AlignIO
from glob import glob

alns = glob("*fas") + glob("*muscle") + glob("*mafft")

hs = []

for a in alns:
	id = a.split(".")[0]
	seqs = AlignIO.read(a, "fasta")
	for seq in seqs:
		if seq.id == "Homo_sapiens":
			un = str(seq.seq).replace("-", "")
			print ">{0}_{1}\n{2}".format(seq.id, id, un)
