% this code base was sued for the paper "Cognitive-Behavioral Predictors of 
% Individual Variability of Functional Connectivity in Healthy Young Adults"
% by Hawco et al. This code is absed on data collected fromt he human
% connectome project. Ina ccordance witht eh data terms and conditions we
% cannot share HCP data directly, thus, unfortunatly, this code is probably
% not easily replicated. 
% 
% We extracted time serives from 360 regions of the glasser atlas, after
% applying a GSR correction tot ehd ata ina ccordance with CHP
% recommentdations (see paper). Time serues data was provided by Jsutin Ng
% and is from his proejct located at https://github.com/14jwkn/FLEXCOG


cd s:\hcp_variability\meants_gsr

% cole anticivic partitionng for network assignment in glasser; good enough
% for government work
nets = textread('D:\programs\ColeAnticevicNetPartition-master\cortex_parcel_network_assignments.txt'); 

%network order, can be sued to sort matricies below
[~, norder] = sort(nets);
% network labels
% Primary Visual 1
% Secondary Visual 2
% Somatomotor 3
% Cingulo-Opercular 4
% Dorsal-attention 5
% Language 6
% Frontoparietal 7
% Auditory 8
% Default 9d
% Posterior Multimodal 10
% Ventral Multimodal 11
% Orbito-Affective 12

%argueably network 10-12 are kinda 'leftovers'

%load data, generated by Justin's code. 
% probably soemwhere burried in https://github.com/14jwkn/FLEXCOG :)
%there is a fodler per eprson, with one csv for each of the potential 4 HCP
%runs. The CSVs are 360 (Glasser ROIs) by 1200 (Time points)
cd(['E:\hcp_variability\meants_gsr\'])
flist = ls;

% find everyone who has 4 runs of data, to precalc matrix size
for pdx = 3:size(flist, 1)
    cd(['E:\hcp_variability\meants_gsr\' flist(pdx,:)]); 
    csvlist = ls('*.csv');
    if size(csvlist, 1) == 4
        inc(pdx) = 1; 
    end
end

%pre allocate conmat, connectivity matricies for each participant
corrmat = zeros(360, 360, sum(inc));

%only included peeps
flist = flist(inc==1,:);

for pdx = 1:size(flist, 1)
    pdx
    cd(['D:\hcp_variability\meants_gsr\' flist(pdx,:)]);
    
    csvlist = ls('*.csv');
    
    % read and concat all 4
    d = [csvread(csvlist(1,:)) csvread(csvlist(2,:)) csvread(csvlist(3,:)) csvread(csvlist(4,:)) ];
    
    % now build the corr matrix
    corrmat(1:360,1:360,pdx) = corr(d');
    
    %corrmat 1 and 2 separate the first 2 scnas and the last 2 scans, for
    %relability meausres later
    d = [csvread(csvlist(1,:)) csvread(csvlist(2,:)) ];
    corrmat1(1:360,1:360,pdx) = corr(d');
    
    d = [csvread(csvlist(3,:)) csvread(csvlist(4,:)) ];
    corrmat2(1:360,1:360,pdx) = corr(d');
    
end
    
% convert matricies to vectors
% this is not an effeicent code and it makes me sad
% NOTE: Current Colin says SUCK IT UP PAST COLIN IT WORKS! 
ind = tril(ones(360,360),-1);
for pdx = 1:size(corrmat,3)
    pdx
    t=corrmat(:,:,pdx);
    convec(pdx,:) = t(ind==1);
    
     t=corrmat1(:,:,pdx);
    convec1(pdx,:) = t(ind==1);
    
     t=corrmat2(:,:,pdx);
    convec2(pdx,:) = t(ind==1);
    
end
% 
% 
% cordist = pdist(convec, 'correlation');
% sdist = squareform(cordist);
% %mean cor dist, not removing motion effects
% mcordis = mean(sdist); 
% 
% cordist = pdist(convec1, 'correlation');
% sdist = squareform(cordist);
% mcordis1 = mean(sdist); 
% 
% cordist = pdist(convec2, 'correlation');
% sdist = squareform(cordist);
% mcordis2 = mean(sdist); 
% 
% [r p] = corr(mcordis1',mcordis2')


%% motion regression
% our firs tpass analysis found a some corelation between
% correlational distance and motion (R=-0.15) during the scan. We thereofre adopted a
% method of regressing motion from each conencitvity edge to further rmeove
% motion effects, separate for eeach edge as motion effects are not
% cosntant across the brain. 
% thisw a svery effective of breaking the relationship between MCD and
% motion
% caution should be sued appling this in 2-group situations (e.g. aptients
% vs controls) where there may be motion differences betwen groups, as some
% group avriance may be removed. happily not the case here. 
cd E:\hcp_variability\r_motion

for pdx = 1:length(flist)
    cd(['E:\hcp_variability\r_motion\' flist(pdx,:)])
    mot_inc(pdx,1:4 ) = [textread('rfMRI_REST1_LR\Movement_RelativeRMS_mean.txt') textread('rfMRI_REST1_RL\Movement_RelativeRMS_mean.txt')  ...
        textread('rfMRI_REST2_LR\Movement_RelativeRMS_mean.txt') textread('rfMRI_REST2_RL\Movement_RelativeRMS_mean.txt')];
end

fd = mean(mot_inc, 2)
figure; hist(fd, 21)

[r p] = corr([rmcordis', fd], 'rows', 'pairwise')


%regress out motion from convec
for idx = 1:size(convec,2)
    B = regress(convec(:,idx),[ ones(size(convec,1),1) fd ])';

    Yhat = sum([ones(size(convec,1),1) fd] .* B, 2); %predicted value
    Yhat = fd * B(2); %predicted value
    resid_convec(:,idx) = convec(:,idx) - Yhat;  %residual after accounting for age
    
    %convec 1 first half of the data
    B = regress(convec(:,idx),[ ones(size(convec,1),1) fd ])';

    Yhat = sum([ones(size(convec1,1),1) fd] .* B, 2); %predicted value
    Yhat = fd * B(2); %predicted value
    resid_convec1(:,idx) = convec1(:,idx) - Yhat;  %residual after accounting for age
    
    % convec 2 second ahlf of the data
    B = regress(convec(:,idx),[ ones(size(convec,1),1) fd ])';

    Yhat = sum([ones(size(convec2,1),1) fd] .* B, 2); %predicted value
    Yhat = fd * B(2); %predicted value
    resid_convec2(:,idx) = convec2(:,idx) - Yhat;  %residual after accounting for age
    
end


% now claculate correlational distance. Fist a distance matrix, then aveage
% across the rows
cordist = pdist((resid_convec), 'correlation');
sdist = squareform(cordist);
%this is our primary outcome avriable
rmcordis = mean(sdist); 

%separate fist and second half
cordist1 = pdist((resid_convec1), 'correlation');
sdist = squareform(cordist1);
%this is our primary outcome avriable
rmcordis1 = mean(sdist); 

cordist2 = pdist((resid_convec2), 'correlation');
sdist = squareform(cordist2);
%this is our primary outcome avriable
rmcordis2 = mean(sdist); 


%% Reliability
%Her ewe take the data from the first 2 scnas, and compare MCD with the
%alst 2 scans
% We correlate MCD values calcualtes across this split oft he data to
% assess how reliable our MCD socres are when we look at differnet scan
% days. 
[r p ] = corr(rmcordis1', rmcordis2')
% Good replicabilityr = 0.5363    p =0.9511e-76

%% Separating pariticpants into high and low variability groups, and 
% comapring network FC and SD 


% simple median plit, 1 = high var, 0  low var groups
gr = (rmcordis > median(rmcordis))'; % 1=high var

% remake connnectivity matrixi for each person on residualized data 
for pdx = 1:size(resid_convec,1)
    rconmat(:,:,pdx) = squareform(resid_convec(pdx,:));
end

% mean network connecitivty by group, figure 1
figure; imagesc(mean(rconmat(norder,norder,gr==0),3), [-.7 .7]);
colormap(redblue)
figure; imagesc(mean(rconmat(norder,norder,gr==1),3), [-.7 .7])
colormap(redblue)
% look ma! no differences  
% (ok there probably are differences jsut hard to see in full matrix)


 %lets look at SD by edge, sorted by network
 % also in FIgure 1
 sh=std(rconmat(norder, norder,gr==1),0,3);
 sl=std(rconmat(norder, norder,gr==0),0,3); 
 
 figure; imagesc(sh,[0 0.2]); colormap(hot)
 figure; imagesc(sl,[0 0.2]);colormap(hot)
%end Figure 1 WHOOT WHOOT! 


% within and between network conn, to make boxplots because I love them. 
for ndx = 1:max(nets)
    for jdx=ndx:max(nets)
        netcon(ndx,jdx,1:size(flist,1)) = squeeze(mean(mean(   (rconmat(nets == ndx, nets == jdx,:)),1),2));
    end
end

%because we have a low of data, we focus on within entwork conencitivty
%between groups, limiting the need for multipel comparisons. 
for idx = 1:12
     [H,P(idx),CI,STATS(idx)] = ttest2(squeeze(netcon(idx,idx,gr==0)), squeeze(netcon(idx,idx,gr==1)));
end

bp = []; grp = []; 
% build the boxplots for figure 2

for idx = 1:12
    bp = [bp; squeeze(netcon(idx,idx,:))];
    grp = [grp; [ ones(length(gr),1)*idx] gr ];%+ ((idx-1)*2)];
end

% initial matlab boxplot for FIgure 2 in the paper, Teo made a
% prettier version in R using the bp and grp variables above  : )
figure; boxplot(bp, grp, 'notch', 'on',  'factorgap', [5 1], 'color', 'k'); 


%%
% cog data on full sample
cd E:\hcp_variability
allcog = csvread('cog_scores.csv');

fid = str2num(flist);
for pdx = 1:length(fid)
    cog(pdx, 1:3) = allcog(find(allcog(:,1) == fid(pdx)), 2:4);  
end

%%
% Ok so at this point we gathered the behavioral variables, and ran the
% regressions in R, Julia's code for that analysis is incldued ina s
% eparate file in the repo

%% Twins and famility analysis
% twins variable and mom and dad IDs added manually into matlab, bad colin
% variable twins has a numberic code, 0 for not, 1 for DZ, 2 for MZ
% fam_subid is the subject IDS for the familty data (e.g. all 1206 pts)

% populate fam_mat, which labels sequential pairs of MZZ, DZ, and non twin
% sibs with unique IDS
% THis was all based on who their mother was. 
       
pid = str2num(flist);
tempmom = mother; 
mz = []; dz = []; sb = []; nsibs = []; 

% this loop is mildly convoluted but it worked, was not easy... 
for pdx =1:length(pid)
    
     id = pid(pdx); 
     
     sid = find(fam_subid == id);
     mid = tempmom(sid); %mother id
     sibs = find(tempmom == mid); %have same mom
     %accunt for fact some twins from lwoer numbers were skipped as not in
     %data
     sibs= sibs(sibs >= sid);
     
     if length(sibs > 1) && tempmom(sid) > 0
         
         for idx = 1:length(sibs)-1
             id2 = find(pid == fam_subid(sibs(idx+1)));
             
             if length(id2) > 0
                 
                 if max(twins(sibs)) > 0
                     if twins(sid) ==2 && twins(sibs(idx+1)) == 2
                         mz(size(mz,1)+1, 1:2) = ([pdx, id2]);
                         tempmom(sibs) = 0;
                     elseif twins(sid) ==1 && twins(sibs(idx+1)) == 1
                         dz(size(dz,1)+1, 1:2) = ([pdx, id2]);
                         tempmom(sibs) = 0;
                     end
                 else
                     sb(size(sb,1)+1, 1:2) = ([pdx, id2]);
                     tempmom(sibs) = 0;
                     sibs(:) = 1; %conviently sub 1 is not in the flist/pid, so this breaks the upper loop. max 1 sibs
                 end
             else 
                 tempmom(sibs(idx+1)) = 0;                 
             end
         end
         
     elseif tempmom(sid) > 0 % no sibs, make a special lsit! 
         nsibs(size(nsibs,1)+1,1) = (pdx);       
     end          
     
     tempmom(sid) = 0; 
end

clear id mid fid sibs                   
              

[H,P,CI,STATS] = ttest2(diff(rmcordis(mz)')', diff(rmcordis(dz)')')
[H,P,CI,STATS] = ttest2(diff(rmcordis(mz)')', diff(rmcordis(sb)')')
[H,P,CI,STATS] = ttest2(diff(rmcordis(dz)')', diff(rmcordis(sb)')')

%   resid_convec
for idx = 1:size(mz,1)
    mz_cordist(idx) = 1 - corr(resid_convec(mz(idx,1), :)', resid_convec(mz(idx,2), :)');
end

for idx = 1:size(dz,1)
    dz_cordist(idx) = 1 - corr(resid_convec(dz(idx,1), :)', resid_convec(dz(idx,2), :)');
end

for idx = 1:size(sb,1)
    sb_cordist(idx) = 1 - corr(resid_convec(sb(idx,1), :)', resid_convec(sb(idx,2), :)');
end

% Make a pairing of random unrelated people
tempmom = mother;
n=1;
for pdx =1:length(pid)
    
    id = pid(pdx);
    
    sid = find(fam_subid == id);
    mid = tempmom(sid);
    
    if mid > 0
        r = 0;
        while r == 0
            r = randi([1 length(pid)]);
            rid = find(fam_subid == pid(r));
            if mid == tempmom(rid) || tempmom(rid) == 0
                r=0
            end
        end
        
        rn_cordist(n) = 1 - corr(resid_convec(pdx, :)', resid_convec(r, :)');
        tempmom([sid rid]) = 0;
        rn_pair(n,1:2) = [pdx r];
        n=n+1;
    end
end


clear P                
[H,P(1),CI,STATS] = ttest2(mz_cordist', dz_cordist')
[H,P(2),CI,STATS] = ttest2(sb_cordist', dz_cordist')
[H,P(3),CI,STATS] = ttest2(mz_cordist', sb_cordist')
[H,P(4),CI,STATS] = ttest2(mz_cordist', rn_cordist')
[H,P(5),CI,STATS] = ttest2(dz_cordist', rn_cordist')
[H,P(6),CI,STATS] = ttest2(sb_cordist', rn_cordist')
fp = fdr(P)

%plot it
alldis = [mz_cordist'; dz_cordist'; sb_cordist'; rn_cordist'];
famgr = [(ones(size(mz,1),1)) ; (ones(size(dz,1),1))*2 ;...
    (ones(size(sb,1),1))*3 ; (ones(size(rn_cordist,2),1))*4 ];

anova1(alldis, famgr)

 
mcor_fams = [diff(rmcordis(mz)')'; diff(rmcordis(dz)')';  diff(rmcordis(sb)')'; diff(rmcordis(rn_pair)')']

anova1(abs(mcor_fams), famgr)
 
clear P
[H,P(1),CI,STATS] = ttest2(abs(diff(rmcordis(mz)')'), abs(diff(rmcordis(dz)')'))
[H,P(2),CI,STATS] = ttest2(abs(diff(rmcordis(mz)')'), abs(diff(rmcordis(sb)')')) 
[H,P(3),CI,STATS] = ttest2(abs(diff(rmcordis(mz)')'), abs(diff(rmcordis(rn_pair)')'))
[H,P(4),CI,STATS] = ttest2(abs(diff(rmcordis(dz)')'), abs(diff(rmcordis(sb)')')) 
[H,P(5),CI,STATS] = ttest2(abs(diff(rmcordis(dz)')'), abs(diff(rmcordis(rn_pair)')'))



