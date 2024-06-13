#[cfg(test)]
mod tests {
    
    use super::super::{*};
    use std::fs;
    use std::fs::File;
    use std::io::Write;
    use std::iter::FromIterator;
    use std::time::Instant;
    use std::collections::HashSet;
    use rand::thread_rng;
    use rand::seq::SliceRandom;

    #[test]
    fn test_1_4x4() -> std::io::Result<()>
    {
        let contents = fs::read_to_string("lists/word_list_500.txt")?;
        let mut words: Vec<String> = Vec::new();
        for w in contents.split('\n') {
            let mut word = String::from(w);
            word.retain(|c| !c.is_whitespace());
            words.push(word);        
        }
        
        let mut rng = thread_rng();
        let mut d01 = vec!['r','i','f','o','b','x']; let mut d02 = vec!['i','f','e','h','e','y']; 
        let mut d03 = vec!['d','e','n','o','w','s']; let mut d04 = vec!['u','t','o','k','n','d']; 
        let mut d05 = vec!['h','m','s','r','a','o']; let mut d06 = vec!['l','u','p','e','t','s']; 
        let mut d07 = vec!['a','c','i','t','o','a']; let mut d08 = vec!['y','l','g','k','u','e']; 
        let mut d09 = vec!['a','a','a','a','a','a']; let mut d10 = vec!['e','h','i','s','p','n']; 
        let mut d11 = vec!['v','e','t','i','g','n']; let mut d12 = vec!['b','a','l','i','y','t']; 
        let mut d13 = vec!['e','z','a','v','n','d']; let mut d14 = vec!['r','a','l','e','s','c']; 
        let mut d15 = vec!['u','w','i','l','r','g']; let mut d16 = vec!['p','a','c','e','m','d']; 

        let start = Instant::now();
        for _ in 0..1000
        {
            d01.shuffle(&mut rng); d02.shuffle(&mut rng); d03.shuffle(&mut rng); d04.shuffle(&mut rng);
            d05.shuffle(&mut rng); d06.shuffle(&mut rng); d07.shuffle(&mut rng); d08.shuffle(&mut rng);
            d09.shuffle(&mut rng); d10.shuffle(&mut rng); d11.shuffle(&mut rng); d12.shuffle(&mut rng);
            d13.shuffle(&mut rng); d14.shuffle(&mut rng); d15.shuffle(&mut rng); d16.shuffle(&mut rng);

            let mut bc = vec![ d01[0], d02[0], d03[0], d04[0], d05[0], d06[0], d07[0], d08[0],
                               d09[0], d10[0], d11[0], d12[0], d13[0], d14[0], d15[0], d16[0]];
            bc.shuffle(&mut rng);

            let mut row1 = String::new(); 
            row1.push(bc[0]); row1.push(bc[1]); row1.push(bc[2]); row1.push(bc[3]);
            let mut row2 = String::new(); 
            row2.push(bc[4]); row2.push(bc[5]); row2.push(bc[6]); row2.push(bc[7]);
            let mut row3 = String::new(); 
            row3.push(bc[8]); row3.push(bc[9]); row3.push(bc[10]); row3.push(bc[11]);
            let mut row4 = String::new(); 
            row4.push(bc[12]); row4.push(bc[13]); row4.push(bc[14]); row4.push(bc[15]);

            let board = [ row1.as_str(), row2.as_str(), row3.as_str(), row4.as_str() ];
            let found = boggle(&board, &words);
        }
        let stop = start.elapsed().as_millis();

        let mut file = File::create("1_score.txt")?;
        let str_out = format!("1: PASSED in {} ms\n", stop);
        file.write_all(str_out.as_bytes())?;

        Ok(())
    }

    #[test]
    fn test_2a_2x2() -> std::io::Result<()>
    {
        let contents = fs::read_to_string("lists/word_list_3000.txt")?;
        let board = [ "ea", "st" ];
        let mut words: Vec<String> = Vec::new();
        for w in contents.split('\n') {
            let mut word = String::from(w);
            word.retain(|c| !c.is_whitespace());
            words.push(word);            
        }
        
        let start = Instant::now();
        let found = boggle(&board, &words);
        let stop = start.elapsed().as_millis();

        let a1 = words_legal(&found, &words);
        assert_eq!(a1, true);
        let a2 = words_in_board(&found, &board);
        assert_eq!(a2, true);
        let a3 = words_coords_ok(&found);
        assert_eq!(a3, true);
        
        let score = get_score(&found);

        let mut file = File::create("2a_score.txt")?;
        let str_out = format!("2a: score = {} in {} ms\n", score, stop);
        file.write_all(str_out.as_bytes())?;

        Ok(())
    }

    #[test]
    fn test_2b_4x4() -> std::io::Result<()>
    {
        let contents = fs::read_to_string("lists/word_list_3000.txt")?;
        let board = [ "isuo", "osve", "nepa", "ntsu" ];
        let mut words: Vec<String> = Vec::new();
        for w in contents.split('\n') {
            let mut word = String::from(w);
            word.retain(|c| !c.is_whitespace());
            words.push(word);            
        }
        
        let start = Instant::now();
        let found = boggle(&board, &words);
        let stop = start.elapsed().as_millis();

        let a1 = words_legal(&found, &words);
        assert_eq!(a1, true);
        let a2 = words_in_board(&found, &board);
        assert_eq!(a2, true);
        let a3 = words_coords_ok(&found);
        assert_eq!(a3, true);
        
        let score = get_score(&found);

        let mut file = File::create("2b_score.txt")?;
        let str_out = format!("2b: score = {} in {} ms\n", score, stop);
        file.write_all(str_out.as_bytes())?;

        Ok(())
    }

    #[test]
    fn test_3a_2x2() -> std::io::Result<()>
    {
        let contents = fs::read_to_string("lists/word_list_scrabble_2019.txt")?;
        let board = [ "ea", "st" ];
        let mut words: Vec<String> = Vec::new();
        for w in contents.split('\n') {
            let mut word = String::from(w);
            word.retain(|c| !c.is_whitespace());
            words.push(word);            
        }
        
        let start = Instant::now();
        let found = boggle(&board, &words);
        let stop = start.elapsed().as_millis();

        let a1 = words_legal(&found, &words);
        assert_eq!(a1, true);
        let a2 = words_in_board(&found, &board);
        assert_eq!(a2, true);
        let a3 = words_coords_ok(&found);
        assert_eq!(a3, true);
        
        let score = get_score(&found);

        let mut file = File::create("3a_score.txt")?;
        let str_out = format!("3a: score = {} in {} ms\n", score, stop);
        file.write_all(str_out.as_bytes())?;

        Ok(())
    }

    #[test]
    fn test_3b_4x4() -> std::io::Result<()>
    {
        let contents = fs::read_to_string("lists/word_list_scrabble_2019.txt")?;
        let board = [ "isuo", "osve", "nepa", "ntsu" ];
        let mut words: Vec<String> = Vec::new();
        for w in contents.split('\n') {
            let mut word = String::from(w);
            word.retain(|c| !c.is_whitespace());
            words.push(word);            
        }
        
        let start = Instant::now();
        let found = boggle(&board, &words);
        let stop = start.elapsed().as_millis();

        let a1 = words_legal(&found, &words);
        assert_eq!(a1, true);
        let a2 = words_in_board(&found, &board);
        assert_eq!(a2, true);
        let a3 = words_coords_ok(&found);
        assert_eq!(a3, true);
        
        let score = get_score(&found);

        let mut file = File::create("3b_score.txt")?;
        let str_out = format!("3b: score = {} in {} ms\n", score, stop);
        file.write_all(str_out.as_bytes())?;

        Ok(())
    }

    #[test]
    fn test_3c_8x8() -> std::io::Result<()>
    {
        let contents = fs::read_to_string("lists/word_list_scrabble_2019.txt")?;
        let board = [ "ocneasra", "crishtir", "llannren", "genssaqn",
                      "damcobnu", "nroosyen", "atsarson", "bessnnis" ];
        let mut words: Vec<String> = Vec::new();
        for w in contents.split('\n') {
            let mut word = String::from(w);
            word.retain(|c| !c.is_whitespace());
            words.push(word);            
        }
        
        let start = Instant::now();
        let found = boggle(&board, &words);
        let stop = start.elapsed().as_millis();

        let a1 = words_legal(&found, &words);
        assert_eq!(a1, true);
        let a2 = words_in_board(&found, &board);
        assert_eq!(a2, true);
        let a3 = words_coords_ok(&found);
        assert_eq!(a3, true);
        
        let score = get_score(&found);

        let mut file = File::create("3c_score.txt")?;
        let str_out = format!("3c: score = {} in {} ms\n", score, stop);
        file.write_all(str_out.as_bytes())?;

        Ok(())
    }

    #[test]
    fn test_3d_16x16() -> std::io::Result<()>
    {
        let contents = fs::read_to_string("lists/word_list_scrabble_2019.txt")?;
        let board = [ "aqoausieartuelro", "lnucrsursdirztom", "qcaclodqtyiycrav", "desmpantsemtdest", 
                      "itqeetroabnoahna", "dnecrpolvnezsmim", "plorssisttugctog", "balvridnmolsbanv",
                      "ojnaoyloifgaesza", "nmellsennpirmcin", "lsalnmucrlarmbam", "pemhzarnyeclpesr",
                      "isnoustaosceicio", "rdeasdoldlesrlom", "pgurlvoclserbmem", "siasnaprmurhtosc" ];
        let mut words: Vec<String> = Vec::new();
        for w in contents.split('\n') {
            let mut word = String::from(w);
            word.retain(|c| !c.is_whitespace());
            words.push(word);            
        }
        
        let start = Instant::now();
        let found = boggle(&board, &words);
        let stop = start.elapsed().as_millis();

        let a1 = words_legal(&found, &words);
        assert_eq!(a1, true);
        let a2 = words_in_board(&found, &board);
        assert_eq!(a2, true);
        let a3 = words_coords_ok(&found);
        assert_eq!(a3, true);
        
        let score = get_score(&found);

        let mut file = File::create("3d_score.txt")?;
        let str_out = format!("3d: score = {} in {} ms\n", score, stop);
        file.write_all(str_out.as_bytes())?;

        Ok(())
    }

    fn words_legal(found: & HashMap<String, Vec<(usize, usize)>>, words: & Vec<String>) -> bool
    {
        let word_set: HashSet<String> = HashSet::from_iter(words.iter().cloned());
        for word in found.keys() {
            let ww = String::from(word);
            if !word_set.contains(&ww) {
                return false;
            }
        }
        true
    }

    fn words_in_board(found: & HashMap<String, Vec<(usize, usize)>>, board: & [&str]) -> bool
    {
        for (k, coords) in found {
            let key_chars: Vec<char> = k.chars().collect();
            if key_chars.len() != coords.len() {
                return false;
            }
            for i in 0..coords.len() {
                if key_chars[i] != get_cell(coords[i], board) {
                    return false
                }
            }
        }
        true
    }

    fn words_coords_ok(found: & HashMap<String, Vec<(usize, usize)>>) -> bool
    {
        for (_, coords) in found {
            for i in 1..coords.len() {
                let (x1, y1) = coords[i-1];
                let (x2, y2) = coords[i];
                let (xd, yd) = ( (x1 as i8-x2 as i8).abs(), (y1 as i8-y2 as i8).abs() );
                if xd > 1 || yd > 1 {
                    return false;
                }
            }
        }
        true
    }

    fn get_score(found: & HashMap<String, Vec<(usize, usize)>>) -> u32
    {
        let mut score = 0;
        let scores = [1, 2, 4, 6, 9, 12, 16, 20];  
        for word in found.keys() {
            let x = (word.len()-1) as usize;
            if x > 7 { score += 20; }
            else { score += scores[x]; }
        }
        score
    }

    fn get_cell(coord: (usize, usize), board: & [&str]) -> char
    {
        let x = coord.0 as usize;
        let y = coord.1 as usize;
        let row = board[x];
        if let Some(c) = row.chars().nth(y) {
            return c;
        }
        return '\0';
    }

}

