import gen_workload as generator
import simulator as simulator
import xlsxwriter

import time




def save2Excel(workbook,worksheet_name, file_name):
    worksheet = workbook.add_worksheet(worksheet_name)
    bold = workbook.add_format({'bold': True})

    worksheet.write('A1', 'PID', bold)
    worksheet.write('B1', 'Arrival Time', bold)
    worksheet.write('C1', 'CPU Burst Time', bold)
    worksheet.write('D1', 'IO Burst Time', bold)
    worksheet.write('E1', 'Bursts Time', bold)
    worksheet.write('F1', 'Turn Around Time', bold)
    worksheet.write('G1', 'Ready Wait Time', bold)
    worksheet.write('H1', 'IO Wait Time', bold)

    file = open(file_name,"r")

    with file as f:
        #read first 3 lines that have no data
        f.readline()
        f.readline()
        f.readline()
        l = f.readline()
        row = 1 #excel row
        while l:           
            vals = [float(x) for x in l.split()]
            worksheet.write(row, 0, vals[0])
            worksheet.write(row, 1, vals[1])
            worksheet.write(row, 2, vals[2])
            worksheet.write(row, 3, vals[3])
            worksheet.write(row, 4, vals[4])
            worksheet.write(row, 5, vals[5])
            worksheet.write(row, 6, vals[6])
            worksheet.write(row, 7, vals[7])
                
            l = f.readline()
            row+=1

    

def parseSimulations():
    fcfs_workbook = xlsxwriter.Workbook("Results/fcfs_results.xlsx")
    sjf_workbook = xlsxwriter.Workbook("Results/sjf_results.xlsx")
    srtf_workbook = xlsxwriter.Workbook("Results/srtf_results.xlsx")
    rr5_workbook = xlsxwriter.Workbook("Results/rr5_results.xlsx")
    rr10_workbook = xlsxwriter.Workbook("Results/rr10_results.xlsx")
    rr15_workbook = xlsxwriter.Workbook("Results/rr15_results.xlsx")

    file = open("Results/simulations.txt","r")
    with file as f:
        l = f.readline()
        while l:
            vals = [str(x) for x in l.split()]
            if vals[1] == 'Results/fcfs_results.xlsx':
                save2Excel(fcfs_workbook,vals[2],vals[0])
            elif vals[1] == 'Results/sjf_results.xlsx':
                save2Excel(sjf_workbook,vals[2],vals[0])
            elif vals[1] == 'Results/srtf_results.xlsx':
                save2Excel(srtf_workbook,vals[2],vals[0])
            elif vals[1] == 'Results/rr5_results.xlsx':
                save2Excel(rr5_workbook,vals[2],vals[0])
            elif vals[1] == 'Results/rr10_results.xlsx':
                save2Excel(rr10_workbook,vals[2],vals[0])
            elif vals[1] == 'Results/rr15_results.xlsx':
                save2Excel(rr15_workbook,vals[2],vals[0])
            else:
                raise ValueError("Unknown workbook")
            l = f.readline()

    fcfs_workbook.close()
    sjf_workbook.close()
    srtf_workbook.close()
    rr5_workbook.close()
    rr10_workbook.close()
    rr15_workbook.close()

def chess_simulations():
    seed = 1
    num_procs = 10
    mean_io_bursts = 40
    mean_iat = 500
    min_CPU = 4
    max_CPU = 6
    min_IO = 5
    max_IO = 10
    scheduler = 'fcfs' #"fcfs", "rr", "sjf", "srtf"
    quantum = None

    f = open("Results/simulations.txt","a+")
    for i in range (30):
        seed = i
        input_file = "Workloads/seed{0}_procs{1}.txt".format(seed,num_procs)
        generator.main(num_procs,mean_io_bursts,mean_iat,min_CPU,max_CPU,min_IO,max_IO,seed,input_file)

        scheduler = 'fcfs'
        output_file = "Simulations/seed{0}_procs{1}_{2}.txt".format(seed,num_procs,scheduler)
        simulator.main(scheduler,quantum,input_file,output_file)
        f.write("{0} {1} {2}\n".format(output_file,"Results/{0}_results.xlsx".format(scheduler),"seed{0}_procs{1}.txt".format(seed,num_procs)))

        scheduler = 'sjf'
        output_file = "Simulations/seed{0}_procs{1}_{2}.txt".format(seed,num_procs,scheduler)
        simulator.main(scheduler,quantum,input_file,output_file)
        f.write("{0} {1} {2}\n".format(output_file,"Results/{0}_results.xlsx".format(scheduler),"seed{0}_procs{1}.txt".format(seed,num_procs)))

        scheduler = 'srtf'
        output_file = "Simulations/seed{0}_procs{1}_{2}.txt".format(seed,num_procs,scheduler)
        simulator.main(scheduler,quantum,input_file,output_file)
        f.write("{0} {1} {2}\n".format(output_file,"Results/{0}_results.xlsx".format(scheduler),"seed{0}_procs{1}.txt".format(seed,num_procs)))

        quantum = 5
        scheduler = 'rr'
        output_file = "Simulations/seed{0}_procs{1}_{2}.txt".format(seed,num_procs,scheduler)
        simulator.main(scheduler,quantum,input_file,output_file)
        f.write("{0} {1} {2}\n".format(output_file,"Results/{0}{1}_results.xlsx".format(scheduler,quantum),"seed{0}_procs{1}.txt".format(seed,num_procs)))

    num_procs = 150
    for i in range (30):
        seed = i
        input_file = "Workloads/seed{0}_procs{1}.txt".format(seed,num_procs)
        generator.main(num_procs,mean_io_bursts,mean_iat,min_CPU,max_CPU,min_IO,max_IO,seed,input_file)

        scheduler = 'fcfs'
        output_file = "Simulations/seed{0}_procs{1}_{2}.txt".format(seed,num_procs,scheduler)
        simulator.main(scheduler,quantum,input_file,output_file)
        f.write("{0} {1} {2}\n".format(output_file,"Results/{0}_results.xlsx".format(scheduler),"seed{0}_procs{1}.txt".format(seed,num_procs)))

        scheduler = 'sjf'
        output_file = "Simulations/seed{0}_procs{1}_{2}.txt".format(seed,num_procs,scheduler)
        simulator.main(scheduler,quantum,input_file,output_file)
        f.write("{0} {1} {2}\n".format(output_file,"Results/{0}_results.xlsx".format(scheduler),"seed{0}_procs{1}.txt".format(seed,num_procs)))

        scheduler = 'srtf'
        output_file = "Simulations/seed{0}_procs{1}_{2}.txt".format(seed,num_procs,scheduler)
        simulator.main(scheduler,quantum,input_file,output_file)
        f.write("{0} {1} {2}\n".format(output_file,"Results/{0}_results.xlsx".format(scheduler),"seed{0}_procs{1}.txt".format(seed,num_procs)))

        quantum = 5
        scheduler = 'rr'
        output_file = "Simulations/seed{0}_procs{1}_{2}.txt".format(seed,num_procs,scheduler)
        simulator.main(scheduler,quantum,input_file,output_file)
        f.write("{0} {1} {2}\n".format(output_file,"Results/{0}{1}_results.xlsx".format(scheduler,quantum),"seed{0}_procs{1}.txt".format(seed,num_procs)))

    num_procs = 500
    for i in range (30):
        seed = i
        input_file = "Workloads/seed{0}_procs{1}.txt".format(seed,num_procs)
        generator.main(num_procs,mean_io_bursts,mean_iat,min_CPU,max_CPU,min_IO,max_IO,seed,input_file)

        scheduler = 'fcfs'
        output_file = "Simulations/seed{0}_procs{1}_{2}.txt".format(seed,num_procs,scheduler)
        simulator.main(scheduler,quantum,input_file,output_file)
        f.write("{0} {1} {2}\n".format(output_file,"Results/{0}_results.xlsx".format(scheduler),"seed{0}_procs{1}.txt".format(seed,num_procs)))

        scheduler = 'sjf'
        output_file = "Simulations/seed{0}_procs{1}_{2}.txt".format(seed,num_procs,scheduler)
        simulator.main(scheduler,quantum,input_file,output_file)
        f.write("{0} {1} {2}\n".format(output_file,"Results/{0}_results.xlsx".format(scheduler),"seed{0}_procs{1}.txt".format(seed,num_procs)))

        scheduler = 'srtf'
        output_file = "Simulations/seed{0}_procs{1}_{2}.txt".format(seed,num_procs,scheduler)
        simulator.main(scheduler,quantum,input_file,output_file)
        f.write("{0} {1} {2}\n".format(output_file,"Results/{0}_results.xlsx".format(scheduler),"seed{0}_procs{1}.txt".format(seed,num_procs)))

        quantum = 5
        scheduler = 'rr'
        output_file = "Simulations/seed{0}_procs{1}_{2}.txt".format(seed,num_procs,scheduler)
        simulator.main(scheduler,quantum,input_file,output_file)
        f.write("{0} {1} {2}\n".format(output_file,"Results/{0}{1}_results.xlsx".format(scheduler,quantum),"seed{0}_procs{1}.txt".format(seed,num_procs)))

    num_procs = 1000
    for i in range (30):
        seed = i
        input_file = "Workloads/seed{0}_procs{1}.txt".format(seed,num_procs)
        generator.main(num_procs,mean_io_bursts,mean_iat,min_CPU,max_CPU,min_IO,max_IO,seed,input_file)

        scheduler = 'fcfs'
        output_file = "Simulations/seed{0}_procs{1}_{2}.txt".format(seed,num_procs,scheduler)
        simulator.main(scheduler,quantum,input_file,output_file)
        f.write("{0} {1} {2}\n".format(output_file,"Results/{0}_results.xlsx".format(scheduler),"seed{0}_procs{1}.txt".format(seed,num_procs)))

        scheduler = 'sjf'
        output_file = "Simulations/seed{0}_procs{1}_{2}.txt".format(seed,num_procs,scheduler)
        simulator.main(scheduler,quantum,input_file,output_file)
        f.write("{0} {1} {2}\n".format(output_file,"Results/{0}_results.xlsx".format(scheduler),"seed{0}_procs{1}.txt".format(seed,num_procs)))

        scheduler = 'srtf'
        output_file = "Simulations/seed{0}_procs{1}_{2}.txt".format(seed,num_procs,scheduler)
        simulator.main(scheduler,quantum,input_file,output_file)
        f.write("{0} {1} {2}\n".format(output_file,"Results/{0}_results.xlsx".format(scheduler),"seed{0}_procs{1}.txt".format(seed,num_procs)))

        quantum = 5
        scheduler = 'rr'
        output_file = "Simulations/seed{0}_procs{1}_{2}.txt".format(seed,num_procs,scheduler)
        simulator.main(scheduler,quantum,input_file,output_file)
        f.write("{0} {1} {2}\n".format(output_file,"Results/{0}{1}_results.xlsx".format(scheduler,quantum),"seed{0}_procs{1}.txt".format(seed,num_procs)))



    f.close()





if __name__ == "__main__":
    #parseSimulations()
    chess_simulations()
    print("DONE")
