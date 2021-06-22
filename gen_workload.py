#!/usr/bin/env python3

# Synopsis
#           ./gen_workload.py num_procs mean_io_bursts mean_iat min_CPU max_CPU min_IO max_IO
# Description
#   Generate workload for CPU scheduler simulation.
#   Interarrival times follow an exponential distribution with mean lambda.
#   CPU and I/O bursts 
#
# Workload format: one line per process, each containing a sequence of
# floating-point numbers of even length. In each line, the first number
# represents the arrival time of the process, and the remaining numbers
# represent the length of the CPU and I/O bursts that result from running
# the process. Since the processes must start and end with a CPU burst, the
# total number of bursts must be odd (and the number of numbers in each line
# must be even).

import sys
import numpy as np
  

def main(num_procs,mean_io_bursts,mean_iat,min_CPU,max_CPU,min_IO,max_IO,r_seed,file_name):
    
        
    f = open(file_name,"wt")

    f.write('# seed = {0}\n'.format(r_seed))
    f.write('# num_procs = {0}\n'.format(num_procs))
    f.write('# mean_io_bursts = {0}\n'.format(mean_io_bursts))
    f.write('# mean_iat = {0}\n'.format(mean_iat))
    f.write('# min_CPU = {0}\n'.format(min_CPU))
    f.write('# max_CPU = {0}\n'.format(max_CPU))
    f.write('# min_IO = {0}\n'.format(min_IO))
    f.write('# max_IO = {0}\n'.format(max_IO))
    
    print("# file = %s" %file_name)
    print("# seed = %d" % r_seed)
    print("# num_procs = %d" % num_procs)
    print("# mean_io_bursts = %g" % mean_io_bursts)
    print("# mean_iat = %d" % mean_iat)
    print("# min_CPU = %g" % min_CPU)
    print("# max_CPU = %g" % max_CPU)
    print("# min_IO = %g" % min_IO)
    print("# max_IO = %g" % max_IO)

    np.random.seed(r_seed)

    t = 0.

    for i in range(num_procs):
        t += np.random.exponential(mean_iat)
        print(t, end=' ')
        f.write('{0} '.format(t))
        io_bursts = np.random.poisson(mean_io_bursts) # Why Poisson? Why not?
        book_play = np.random.randint(10,12)
        for j in range(io_bursts):
            burst = np.random.uniform(min_CPU, max_CPU)
            if j > book_play and io_bursts-j>5:
                burst = burst*np.random.uniform(4, 6)
            print(burst, end=' ')
            f.write('{0} '.format(burst))
            burst = np.random.uniform(min_IO, max_IO)
            print(burst, end=' ')
            f.write('{0} '.format(burst))
        burst = np.random.uniform(min_CPU, max_CPU)
        print(burst)
        f.write('{0}\n'.format(burst))

    f.close()


if __name__ == "__main__":
    if len(sys.argv) == 10:
        num_procs = int(sys.argv[1])
        mean_io_bursts = int(sys.argv[2])
        mean_iat = float(sys.argv[3])
        min_CPU = float(sys.argv[4])
        max_CPU = float(sys.argv[5])
        min_IO = float(sys.argv[6])
        max_IO = float(sys.argv[7])
        r_seed = int(sys.argv[8])
        file_name = sys.argv[9]       
        main(num_procs,mean_io_bursts,mean_iat,min_CPU,max_CPU,min_IO,max_IO,r_seed,file_name)
        
    else:
        raise Exception("The number of arguments should be 9.")

